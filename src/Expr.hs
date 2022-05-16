module Expr where

import Control.Applicative
import qualified Data.HashMap.Strict as H
import qualified Type

data Context = Context
  { -- variable registered as of course variable
    axioms :: H.HashMap String Expr,
    variable :: H.HashMap String Expr -- other variable
  }

retrieveVariable :: Context -> String -> Maybe (Context, Expr)
retrieveVariable ctx name =
  ( (ctx {variable = H.delete name $ variable ctx},)
      <$> H.lookup name (variable ctx)
  )
    <|> ( (ctx,) . OfCourse <$> H.lookup name (axioms ctx)
        )

data Expr
  = BindedTypeVar Int
  | Unit
  | PlusExpr Type.Type String Expr
  | Match Expr (H.HashMap String (String, Expr))
  | WithExpr (H.HashMap String Expr) Context
  | Select Expr String
  | Record (H.HashMap String Expr)
  | Tuple [Expr]
  | Proc Int Type.Type [Instruction] Context -- Int : unique global indentifier
  | Recursive Expr -- folded expr
  | Forall Expr -- contains binded variable
  | OfCourse Expr -- contains no non axioms variable
  | BuiltinFunc String Type.Type (Context -> [Expr] -> IO (Either () Expr)) -- Proc of type Proc [In(A), ..., In(B), Out(C)]s

data Instruction
  = Recv String String -- var <- proc;
  | Send String String -- var -> proc;
  -- let name : ty = proc begin ... end;
  | DeclareProc String String Type.Type [Instruction] Context
  | -- let * = expr;
    DestructUnit String
  | -- let (x, y, z) = expr;
    DestructTuple Expr [String]
  | Copy String String -- copy of_course_name as var;
  | Drop String -- drop axiom_name;
  -- let name = expr;
  -- move all used variables
  | Assign String Expr

instance (Show Expr) where
  show = \case
    BindedTypeVar n -> "var " ++ show n
    Unit -> "*"
    PlusExpr ty member e -> member ++ "<" ++ show ty ++ ">(" ++ show e ++ ")"
    Match e branches ->
      "match " ++ show e ++ "{\n"
        ++ H.foldMapWithKey (\name value -> name ++ " => " ++ show value ++ "\n") branches
        ++ "}"
    WithExpr branches _ctx ->
      "&{\n"
        ++ H.foldMapWithKey (\name value -> name ++ " => " ++ show value ++ "\n") branches
        ++ "}"
    Select e name -> "select[" ++ name ++ "](" ++ show e ++ ")"
    Record fields ->
      "struct{\n"
        ++ H.foldMapWithKey (\name value -> name ++ " => " ++ show value ++ "\n") fields
        ++ "}"
    Tuple elements ->
      "("
        ++ foldr
          ( \e rest ->
              rest ++ ", " ++ show e
          )
          ""
          elements
        ++ ")"
    Proc idx ty _ _ -> "proc " ++ show idx ++ " : " ++ show ty
    Recursive e -> "fold " ++ show e
    Forall e -> "forall . " ++ show e
    OfCourse e -> "!" ++ show e
    BuiltinFunc name ty _ -> "builtin " ++ name ++ " : " ++ show ty
