{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module AST where

import Data.Hashable
import GHC.Generics

-- For handling renaming
data BinTree =
  Root Int
  | L BinTree
  | R BinTree deriving (Eq, Generic, Hashable)

type Ident = BinTree

{-
rootVal :: BinTree -> Int
rootVal (Root x) = x
rootVal (L t) = rootVal t
rootVal (R t) = rootVal t
-}

data Expr
  = Var Ident
  | Unit
  | Perp
  | Tensor Expr Expr
  | Par Expr Expr
  | Inl Expr
  | Inr Expr
  | With [Ident] ProofExpr ProofExpr
  | WhyNot Expr
  | Empty
  | Concat Expr Expr
  | OfCourse [Ident] ProofExpr
  deriving (Eq)

data ProofExpr = ProofExpr [Coequation] [Expr]
  deriving (Eq)

data Coequation = Coequation Expr Expr
  deriving (Eq)

data SyntacticExpr
  = SExpr Expr
  | SProofExpr ProofExpr
  | SCoequation Coequation

{-
merge :: Eq a => [a] -> [a] -> [a]
merge x y = x ++ filter (`notElem` x) y

mergeL :: (Foldable t, Eq a) => t [a] -> [a]
mergeL = foldr merge []

activeNames :: SyntacticExpr -> [Ident]
activeNames (SCoequation (Coequation t u)) =
  merge (activeNames $ SExpr t) (activeNames $ SExpr u)
activeNames (SProofExpr (ProofExpr c t)) = merge
  (mergeL $ fmap (activeNames . SCoequation) c)
  (mergeL $ fmap (activeNames . SExpr) t)
activeNames (SExpr (Var name)) = [name]
activeNames (SExpr Unit) = []
activeNames (SExpr Perp) = []
activeNames (SExpr (Tensor t u)) =
  merge (activeNames $ SExpr t) (activeNames $ SExpr u)
activeNames (SExpr (Par t u)) =
  merge (activeNames $ SExpr t) (activeNames $ SExpr u)
activeNames (SExpr (Inl t)) = activeNames (SExpr t)
activeNames (SExpr (Inr u)) = activeNames (SExpr u)
activeNames (SExpr (With x p q)) =
  filter (`notElem` x) $
  merge (activeNames $ SProofExpr p) (activeNames $ SProofExpr q)
activeNames (SExpr (WhyNot t)) = activeNames (SExpr t)
activeNames (SExpr Empty) = []
activeNames (SExpr (Concat t u)) =
 merge (activeNames $ SExpr t) (activeNames $ SExpr u)
activeNames (SExpr (OfCourse x p)) =
  filter (`notElem` x) $ activeNames $ SProofExpr p
-}

-- add L if False, add R if True, to all Idents in the expression
ren :: Bool -> SyntacticExpr -> SyntacticExpr
ren b (SProofExpr (ProofExpr theta t)) =
  SProofExpr $ ProofExpr
    (fmap (extractCoequation . ren b . SCoequation) theta)
    (fmap (extractExpr . ren b . SExpr) t)
ren b (SCoequation (Coequation t u)) =
  SCoequation $ Coequation
    (extractExpr . ren b . SExpr $ t)
    (extractExpr . ren b . SExpr $ u)
ren b (SExpr (Var name)) =
  if b
  then
    SExpr . Var . L $ name
  else
    SExpr . Var . R $ name
ren _ (SExpr Unit) = SExpr Unit
ren _ (SExpr Perp) = SExpr Perp
ren b (SExpr (Tensor t u)) =
  SExpr $ Tensor
    (extractExpr . ren b . SExpr $ t)
    (extractExpr . ren b . SExpr $ u)
ren b (SExpr (Par t u)) =
  SExpr $ Par
    (extractExpr . ren b . SExpr $ t)
    (extractExpr . ren b . SExpr $ u)
ren b (SExpr (Inl t)) =
  SExpr . Inl . extractExpr . ren b . SExpr $ t
ren b (SExpr (Inr u)) =
  SExpr . Inr . extractExpr . ren b . SExpr $ u
ren b (SExpr (With x p q)) =
  SExpr $ With
    (fmap (\name ->
      let SExpr (Var name') = ren b . SExpr . Var $ name
      in name'
    ) x)
    (extractProofExpr . ren b . SProofExpr $ p)
    (extractProofExpr . ren b . SProofExpr $ q)
ren b (SExpr (WhyNot t)) =
  SExpr . WhyNot . extractExpr . ren b . SExpr $ t
ren _ (SExpr Empty) = SExpr Empty
ren b (SExpr (Concat t u)) =
  SExpr $ Concat
    (extractExpr . ren b . SExpr $ t)
    (extractExpr . ren b . SExpr $ u)
ren b (SExpr (OfCourse x p)) =
  SExpr $ OfCourse
    (fmap (\name ->
      let SExpr (Var name') = ren b . SExpr . Var $ name
      in name'
    ) x)
    (extractProofExpr . ren b . SProofExpr $ p)

extractExpr :: SyntacticExpr -> Expr
extractExpr (SExpr e) = e
extractExpr _ = undefined

extractProofExpr :: SyntacticExpr -> ProofExpr
extractProofExpr (SProofExpr p) = p
extractProofExpr _ = undefined

extractCoequation :: SyntacticExpr -> Coequation
extractCoequation (SCoequation c) = c
extractCoequation _ = undefined

substitute :: Ident -> Expr -> SyntacticExpr -> SyntacticExpr
substitute idx var (SProofExpr (ProofExpr theta t)) =
  SProofExpr $ ProofExpr
    (fmap (extractCoequation . substitute idx var . SCoequation) theta)
    (fmap (extractExpr . substitute idx var . SExpr) t)
substitute idx var (SCoequation (Coequation t u)) =
  SCoequation $ Coequation
    (extractExpr . substitute idx var . SExpr $ t)
    (extractExpr . substitute idx var . SExpr $ u)
substitute idx var (SExpr (Var name)) =
  SExpr $ if idx == name
    then var
    else Var name
substitute _ _ (SExpr Unit) = SExpr Unit
substitute _ _ (SExpr Perp) = SExpr Perp
substitute idx var (SExpr (Tensor t u)) =
  SExpr $ Tensor
    (extractExpr . substitute idx var . SExpr $ t)
    (extractExpr . substitute idx var . SExpr $ u)
substitute idx var (SExpr (Par t u)) =
  SExpr $ Par
    (extractExpr . substitute idx var . SExpr $ t)
    (extractExpr . substitute idx var . SExpr $ u)
substitute idx var (SExpr (Inl t)) =
  SExpr . Inl . extractExpr . substitute idx var . SExpr $ t
substitute idx var (SExpr (Inr u)) =
  SExpr . Inr . extractExpr . substitute idx var . SExpr $ u
substitute idx var (SExpr (With x p q)) =
  if idx `elem` x
  then SExpr $ With x p q
  else SExpr $ With x
    (extractProofExpr . substitute idx var . SProofExpr $ p)
    (extractProofExpr . substitute idx var . SProofExpr $ q)
substitute idx var (SExpr (WhyNot t)) =
  SExpr . extractExpr . substitute idx var . SExpr $ t
substitute idx var (SExpr (Concat t u)) =
  SExpr $ Concat
    (extractExpr . substitute idx var . SExpr $ t)
    (extractExpr . substitute idx var . SExpr $ u)
substitute _ _ (SExpr Empty) = SExpr Empty
substitute idx var (SExpr (OfCourse x p)) =
  if idx `elem` x
  then SExpr $ OfCourse x p
  else SExpr . OfCourse x . extractProofExpr . substitute idx var . SProofExpr $ p
