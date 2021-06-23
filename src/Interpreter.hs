module Interpreter where

import AST
import qualified Data.HashMap.Strict as H
import Data.Sequence (Seq (..), (><))
import qualified Data.Sequence as S

data Interpreter = Interpreter (Seq Coequation) (H.HashMap Ident Expr)

-- add symetric for every case
step :: Interpreter -> Interpreter
-- Communication
step (Interpreter (Coequation (Var name ty) t :<| h) ctx) =
  case H.lookup name ctx of
    Just u ->
      Interpreter (h :|> Coequation t u) (H.delete name ctx)
    Nothing ->
      Interpreter h (H.insert name t ctx)
-- Unit
step (Interpreter (Coequation Unit Perp :<| h) ctx) = Interpreter h ctx
-- Pair
step (Interpreter (Coequation (Tensor t u) (Par t' u') :<| h) ctx) = Interpreter (h :|> Coequation t t' :|> Coequation u u') ctx
-- Case Left
step (Interpreter (Coequation (With x (ProofExpr theta (t : ts)) (ProofExpr ksi (u : us))) (Inl v) :<| h) ctx) =
  Interpreter ((h >< S.fromList theta >< S.fromList (zipWith ((. snd) . Coequation . snd) x ts)) :|> Coequation (snd t) v) ctx
-- Default : pass
step (Interpreter (c :<| h) ctx) = step (Interpreter (h :|> c) ctx)

-- TODO: move all lists to seq ? (in AST)
