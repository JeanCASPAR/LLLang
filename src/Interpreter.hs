module Interpreter where

import AST
import qualified Data.HashMap.Strict as H
import Data.Sequence (Seq (..), (><))
import qualified Data.Sequence as S
import Data.Foldable (toList)

data Interpreter = Interpreter (Seq Coequation) (H.HashMap Ident Expr)

step :: Interpreter -> Interpreter
-- Communication
step (Interpreter (Coequation (Var name) t :<| h) ctx) =
  case H.lookup name ctx of
    Just u ->
      Interpreter (h :|> Coequation t u) (H.delete name ctx)
    Nothing ->
      Interpreter h (H.insert name t ctx)
step (Interpreter (Coequation t (Var name) :<| h) ctx) =
  case H.lookup name ctx of
    Just u ->
      Interpreter (h :|> Coequation t u) (H.delete name ctx)
    Nothing ->
      Interpreter h (H.insert name t ctx)
-- Unit
step (Interpreter (Coequation Unit Perp :<| h) ctx) = Interpreter h ctx
step (Interpreter (Coequation Perp Unit :<| h) ctx) = Interpreter h ctx
-- Pair
step (Interpreter (Coequation (Tensor t u) (Par t' u') :<| h) ctx) = Interpreter (h :|> Coequation t t' :|> Coequation u u') ctx
step (Interpreter (Coequation (Par t' u') (Tensor t u) :<| h) ctx) = Interpreter (h :|> Coequation t t' :|> Coequation u u') ctx
-- Case Left
step (Interpreter (Coequation (With x (ProofExpr theta (t : ts)) (ProofExpr ksi (u : us))) (Inl v) :<| h) ctx) =
  Interpreter ((h >< S.fromList theta >< S.fromList (zipWith (Coequation . Var) x ts)) :|> Coequation t v) ctx
step (Interpreter (Coequation (Inl v) (With x (ProofExpr theta (t : ts)) (ProofExpr ksi (u : us))) :<| h) ctx) =
  Interpreter ((h >< S.fromList theta >< S.fromList (zipWith (Coequation . Var) x ts)) :|> Coequation t v) ctx
-- Case Right
step (Interpreter (Coequation (With x (ProofExpr theta (t : ts)) (ProofExpr ksi (u : us))) (Inr v) :<| h) ctx) =
  Interpreter ((h >< S.fromList ksi >< S.fromList (zipWith (Coequation . Var) x us)) :|> Coequation u v) ctx
step (Interpreter (Coequation (Inr v) (With x (ProofExpr theta (t : ts)) (ProofExpr ksi (u : us))) :<| h) ctx) =
  Interpreter ((h >< S.fromList ksi >< S.fromList (zipWith (Coequation . Var) x us)) :|> Coequation u v) ctx
-- Read
step (Interpreter (Coequation (OfCourse x (ProofExpr theta (t : ts))) (WhyNot u) :<| h) ctx) =
  Interpreter ((h >< S.fromList theta >< S.fromList (zipWith (Coequation . Var) x ts)) :|> Coequation t u) ctx
step (Interpreter (Coequation (WhyNot u) (OfCourse x (ProofExpr theta (t : ts))) :<| h) ctx) =
  Interpreter ((h >< S.fromList theta >< S.fromList (zipWith (Coequation . Var) x ts)) :|> Coequation t u) ctx
-- Discard
step (Interpreter (Coequation (OfCourse x p) AST.Empty :<| h) ctx) =
  Interpreter (h >< S.fromList (fmap ((`Coequation` AST.Empty) . Var) x)) ctx
step (Interpreter (Coequation AST.Empty (OfCourse x p) :<| h) ctx) =
  Interpreter (h >< S.fromList (fmap ((`Coequation` AST.Empty) . Var) x)) ctx
-- Copy
step (Interpreter (Coequation (OfCourse x p) (Concat u v) :<| h) ctx) =
  Interpreter ((h >< S.fromList (zipWith (Coequation . Var) x 
    (zipWith (\x y -> Concat (Var (L x)) (Var (R y))) x x)))
    :|> Coequation (extractExpr . ren True . SExpr $ OfCourse x p) u
    :|> Coequation (extractExpr . ren False .SExpr $ OfCourse x p) v
  ) ctx
step (Interpreter (Coequation (Concat u v) (OfCourse x p) :<| h) ctx) =
  Interpreter ((h >< S.fromList (zipWith (Coequation . Var) x 
    (zipWith (\x y -> Concat (Var (L x)) (Var (R y))) x x)))
    :|> Coequation (extractExpr . ren True . SExpr $ OfCourse x p) u
    :|> Coequation (extractExpr . ren False .SExpr $ OfCourse x p) v
  ) ctx
-- Default : pass
step (Interpreter (c :<| h) ctx) = Interpreter (h :|> c) ctx
step (Interpreter S.Empty ctx) = Interpreter S.Empty ctx

cleanup :: ProofExpr -> ProofExpr
cleanup (ProofExpr (Coequation (Var name) t : theta) ts) =
  ProofExpr theta $ fmap (extractExpr . substitute name t . SExpr) ts
cleanup (ProofExpr (Coequation t (Var name) : theta) ts) =
  ProofExpr theta $ fmap (extractExpr . substitute name t . SExpr) ts
cleanup (ProofExpr (t : theta) ts) =
  let (ProofExpr theta' ts') = cleanup (ProofExpr theta ts) in
  ProofExpr (t : theta') ts'
cleanup (ProofExpr [] ts) = ProofExpr [] ts

-- Run the interpreter until the end of the evaluation
run :: Interpreter -> Interpreter
run (Interpreter theta ctx) = undefined

compute :: ProofExpr -> ProofExpr
compute (ProofExpr theta t) =
  let (Interpreter theta' ctx) = run $ Interpreter (S.fromList theta) H.empty
      theta'' = toList theta' ++ H.foldrWithKey (
          \name val acc -> Coequation (Var name) val : acc
        ) [] ctx
  in cleanup (ProofExpr theta'' t)