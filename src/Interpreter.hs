module Interpreter
where

import qualified Expr
import qualified Types
import qualified Data.HashMap.Strict as H
import Data.Functor (($>))
import qualified Data.Atomics.Counter as AC
import qualified Data.Sequence as Seq
import Control.Concurrent
import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent.STM.TVar as STM.TVar
import qualified Control.Concurrent.STM.TQueue as STM.TQueue
import Data.Function

data Interpreter = Interpreter {
    counter :: AC.AtomicCounter,
    pool :: Pool
}

data Proc = Proc {
    index :: Int,
    procExpr :: Expr.Expr, -- should be a proc
    inputs :: Seq.Seq Expr.Expr,
    outputs :: Seq.Seq Expr.Expr,
    state :: State
}

data State = WaitInput | RemainingOutputs | WaitOfOther | End

type Pool = [STM.TVar.TVar Proc]

newInterpreter :: IO Interpreter
newInterpreter = do
  counter <- AC.newCounter 0
  pure $ Interpreter {
    counter,
    pool = []
  }

execProc :: Interpreter -> Proc -> IO (STM.TVar.TVar Proc)
execProc interpreter process = do
  (idx, ty, instructions, ctx) <- case procExpr process of
    Expr.Proc idx ty instructions ctx -> pure (idx, ty, instructions, ctx)
    _ -> fail "not a fully evaluated process"

  case instructions of
    instr:rest -> do
      res <- evalInstruction ctx instr
      case res of
        Error -> fail "unknown error"
        NewContext newCtx -> execProc interpreter
          process {procExpr = Expr.Proc idx ty rest newCtx}
        TakeInput name -> case inputs process of
          restInputs Seq.:|> input -> do
            (newCtx, newTy) <- case ty of
              Types.Proc ((Types.In, Types.OfCourse _) : restTypes) ->
                pure (ctx {Expr.axioms = H.insert name input $ Expr.axioms ctx}, restTypes)
              Types.Proc ((Types.In, _) : restTypes) ->
                pure (ctx {Expr.variable = H.insert name input $ Expr.variable ctx}, restTypes)
              Types.Proc ((Types.Out, _) : _) -> fail "expected output"
              Types.Proc [] -> fail "empty process"
              _ -> fail "unknown type"
            execProc interpreter process {
              procExpr = Expr.Proc idx (Types.Proc newTy) rest newCtx,
              inputs = restInputs
            }
          Seq.Empty -> STM.TVar.newTVarIO process {
            state = WaitInput
          }
        SendOutput name -> do
          (newCtx, expr) <- Expr.retrieveVariable ctx name
            & maybe (fail $ "unknown name " ++ name) pure
          case ty of
            Types.Proc ((Types.Out, _):newTy) ->
              execProc interpreter process {
                procExpr = Expr.Proc idx (Types.Proc newTy) rest newCtx,
                outputs = expr Seq.:<| outputs process
              }
            Types.Proc ((Types.In, _):_) -> fail "expected input"
            Types.Proc [] -> fail "empty process"
            _ -> fail "unknown type"
        SendTo varName procName -> do
          (fstCtx, expr) <- Expr.retrieveVariable ctx varName
            & maybe (fail $ "unknown name " ++ varName) pure
          (sndCtx, procExpr) <- (
            (ctx {Expr.variable = H.delete procName $ Expr.variable fstCtx},)
              <$> H.lookup procName (Expr.variable fstCtx))
            & maybe (fail $ "unknow name " ++ procName) pure
          case procExpr of
            Expr.Proc procId (Types.Proc ((Types.Out, _):newTy)) procInstr procCtx ->
              let
                updater :: STM.TVar.TVar Proc -> IO ()
                updater targetProc = STM.atomically $ STM.TVar.modifyTVar targetProc
                  \otherProcess -> if index otherProcess == procId
                    then otherProcess { inputs = expr Seq.:<| inputs otherProcess }
                    else otherProcess
                newCtx = if null newTy
                  then sndCtx
                  else sndCtx {
                    Expr.variable = H.insert
                      procName
                      (Expr.Proc procId (Types.Proc newTy) procInstr procCtx) $
                      Expr.variable sndCtx
                  }
              in do
                mapM_ updater (pool interpreter)
                execProc interpreter process {
                  procExpr = Expr.Proc idx ty rest newCtx
                }
            _ -> fail "unknown error"
        RecvFrom _ _ -> _
    [] -> case outputs process of
      Seq.Empty -> STM.newTVarIO process {
        state = End
      }
      _ -> STM.newTVarIO process {
        state = RemainingOutputs
      }


data InstrResult = Error |
  NewContext Expr.Context |
  -- input/output from/to self
  TakeInput String | SendOutput String |
  -- input/output var from/in proc
  SendTo String String | RecvFrom String String

evalInstruction :: Expr.Context -> Expr.Instruction -> IO InstrResult
evalInstruction ctx = \case
  Expr.DestructUnit var -> case Expr.variable ctx H.!? var of
          Just e -> do
            res <- evalExpr ctx e
            case res of
              Right (_, Expr.Unit) -> pure . NewContext $ ctx
              _ -> pure Error
          Nothing -> pure Error
  Expr.DestructTuple tuple names -> do
    res <- evalExpr ctx tuple
    case res of
      Right (newCtx, Expr.Tuple l) ->
        let hashmap = H.fromList $ zip names l in
            pure . NewContext $ newCtx {
              Expr.variable = Expr.variable newCtx `H.union` hashmap
            }
      _ -> pure Error
  Expr.Copy of_course new_var -> do
    case Expr.axioms ctx H.!? of_course of
      Just (Expr.OfCourse expr) -> pure . NewContext $
        ctx {Expr.variable = H.insert new_var expr (Expr.variable ctx)}
      _ -> pure Error
  _ -> undefined

-- should eval until the end
evalExpr :: Expr.Context -> Expr.Expr -> IO (Either () (Expr.Context, Expr.Expr))
evalExpr ctx = \case
  Expr.Match e branches -> do
      res <- evalExpr ctx e
      case res of
          Right (_, Expr.PlusExpr _ branch innerPlusExpr) -> case branches H.!? branch of
            Just (name, branchExpr) -> evalExpr (
                ctx {Expr.variable=H.insert name innerPlusExpr (Expr.variable ctx)}
              )
              branchExpr
            Nothing -> pure $ Left ()
          _ -> pure $ Left ()
  Expr.Select expr branch -> do
      res <- evalExpr ctx expr
      case res of
          Right (_, Expr.WithExpr branches innerCtx) -> case branches H.!? branch of
            Just branchExpr -> evalExpr (
                ctx {Expr.axioms=Expr.axioms innerCtx, Expr.variable=Expr.variable innerCtx}
              )
              branchExpr
            Nothing -> pure $ Left ()
          _ -> pure $ Left ()
  e -> pure . Right $ (ctx, e)

builtinPrint :: Expr.Expr
builtinPrint = Expr.OfCourse . Expr.Forall $ Expr.BuiltinFunc "print" (Types.Proc [
    (Types.In, Types.Variable 1), (Types.Out, Types.One)
    ]) $
    \ctx -> \case
        [x] -> (evalExpr ctx x >>= either pure (print . snd)) $> Right Expr.Unit
        _ -> print "print take only one argument" $> Left ()

builtinExit :: Expr.Expr
builtinExit = Expr.OfCourse . Expr.Forall $ Expr.BuiltinFunc "exit" (Types.Proc [
    (Types.In, Types.One), (Types.Out, Types.Variable 1)
    ]) . const . const . pure $ Left ()