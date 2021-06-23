module AST where

-- equality should be handled by hand

type Ident = String

data Type
  = TNamed Ident
  | TWith Type Type
  | TPlus Type Type
  | TTensor Type Type
  | TPar Type Type
  | TWhyNot Type
  | TOfCourse Type
  | TFun Type Type
  | TZero
  | TOne
  | TTop
  | TBottom
  | TForall Ident Type -- forall a. A
  | TExists Ident Type -- exists a. A
  deriving (Eq)

data Expr
  = Var Ident Type
  | Unit
  | Perp
  | Tensor Expr Expr
  | Par Expr Expr
  | Inl Expr
  | Inr Expr
  | With [(Ident, Expr)] ProofExpr ProofExpr
  | WhyNot Expr
  | Empty
  | Concat Expr Expr
  | OfCourse [(Ident, Expr)] ProofExpr
  deriving (Eq)

data ProofExpr = ProofExpr [Coequation] [(Ident, Expr)]
  deriving (Eq)

data Coequation = Coequation Expr Expr
  deriving (Eq)

data SyntacticExpr
  = SExpr Expr
  | SProofExpr ProofExpr
  | SCoequation Coequation

merge :: Eq a => [a] -> [a] -> [a]
merge x y = x ++ filter (`notElem` x) y

mergeL :: Foldable t => Eq a => t [a] -> [a]
mergeL = foldr merge []

reaction :: [Coequation] -> Maybe [Coequation]
reaction (Coequation a (Var x _) : Coequation (Var y _) d : h)
  | x == y =
    Just $ Coequation a d : h -- Communication
reaction (Coequation Unit Perp : h) = Just h -- Unit
reaction (Coequation (Tensor t u) (Par t' u') : h) = Just $ Coequation t t' : Coequation u u' : h -- Pair
reaction (Coequation (With x (ProofExpr theta (t : ts)) (ProofExpr ksi (u : us))) (Inl v) : h) =
  Just $ Coequation (snd t) v : zipWith ((. snd) . Coequation . snd) x ts ++ theta ++ h -- Case Left
reaction (Coequation (With x (ProofExpr theta (t : ts)) (ProofExpr ksi (u : us))) (Inr v) : h) =
  Just $ Coequation (snd u) v : zipWith ((. snd) . Coequation . snd) x us ++ ksi ++ h -- Case Right
reaction (Coequation (OfCourse x (ProofExpr theta (t : ts))) (WhyNot u) : h) =
  Just $ Coequation (snd t) u : zipWith ((. snd) . Coequation . snd) x ts ++ theta ++ h -- Read
reaction (Coequation (OfCourse x p) Empty : h) = Just $ fmap ((flip $ Coequation . snd) Empty) x ++ h -- Discard
reaction (Coequation (OfCourse x p) (Concat u v) : h) = Just undefined -- Copy (should implement renaming first)
reaction _ = Nothing

cleanup :: ProofExpr -> Maybe ProofExpr
cleanup = undefined -- (should implement active name first)
