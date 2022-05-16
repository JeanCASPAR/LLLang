module Type
  ( Type (..),
    Direction (..),
    specialize,
    fold,
    unfold,
  )
where

import Data.Bifunctor (second)
import qualified Data.HashMap.Strict as H

data Direction = In | Out deriving (Eq, Show)

data Type
  = -- Type variable
    Variable Int -- >= 1
    -- Units
  | Zero
  | Top
  | One
  | -- Named agregate
    Plus (H.HashMap String Type)
  | With (H.HashMap String Type)
  | Record (H.HashMap String Type)
  | -- Unnamed agregate
    Tuple [Type]
  | Proc [(Direction, Type)]
  | -- Recursive
    Recursive Type
  | -- Forall
    Forall Type
  | -- Of Course
    OfCourse Type
  deriving (Eq, Show)

-- shift all free variables by n, where there is n binder left to ty
shift :: Int -> Type -> Type
shift n = aux 0
  where
    aux :: Int -> Type -> Type
    aux k = \case
      Variable n' | n' > k -> Variable (n' + n) -- Free vars
      Plus m -> Plus $ H.map (aux n) m
      With m -> With $ H.map (aux n) m
      Record m -> Record $ H.map (aux n) m
      Tuple l -> Tuple $ fmap (aux n) l
      Proc l -> Proc $ fmap (second $ aux n) l
      Recursive ty' -> Recursive $ aux (n + 1) ty'
      Forall ty' -> Forall $ aux (n + 1) ty'
      OfCourse ty' -> OfCourse $ aux n ty'
      ty' -> ty'

-- Specialize forall type
specialize :: Type -> Type -> Maybe Type
specialize (Forall ty) replacement = Just $ aux 1 ty
  where
    -- Number of binders
    aux :: Int -> Type -> Type
    aux n = \case
      Variable n' | n == n' -> shift (n - 1) replacement
      Variable n' | n < n' -> Variable (n' - 1) -- Free vars
      Plus m -> Plus $ H.map (aux n) m
      With m -> With $ H.map (aux n) m
      Record m -> Record $ H.map (aux n) m
      Tuple l -> Tuple $ fmap (aux n) l
      Proc l -> Proc $ fmap (second $ aux n) l
      Recursive ty' -> Recursive $ aux (n + 1) ty'
      Forall ty' -> Forall $ aux (n + 1) ty'
      OfCourse ty' -> OfCourse $ aux n ty'
      ty' -> ty'
specialize _ _ = Nothing

-- Fold recursive type :
-- A[mu a.A/a] -> mu a.A :
-- Take mu a. A and A[mu a.A/a], check if the latter is the former unfolded and return the result
fold :: Type -> Type -> Bool
fold folded unfolded = unfold folded == Just unfolded

-- Unfold recursive type :
-- mu a.A -> A[mu a.A/a]
unfold :: Type -> Maybe Type
unfold (Recursive ty) = Just $ aux 1 ty
  where
    -- Number of binders
    aux :: Int -> Type -> Type
    aux n = \case
      Variable n' | n == n' -> shift (n - 1) (Recursive ty)
      Variable n' | n < n' -> Variable (n' - 1) -- Free vars
      Plus m -> Plus $ H.map (aux n) m
      With m -> With $ H.map (aux n) m
      Record m -> Record $ H.map (aux n) m
      Tuple l -> Tuple $ fmap (aux n) l
      Proc l -> Proc $ fmap (second $ aux n) l
      Recursive ty' -> Recursive $ aux (n + 1) ty'
      Forall ty' -> Forall $ aux (n + 1) ty'
      OfCourse ty' -> OfCourse $ aux n ty'
      ty' -> ty'
unfold _ = Nothing
