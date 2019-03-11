{-# language DataKinds #-}
{-# language KindSignatures #-}
{-# language GADTs #-}
{-# language PolyKinds #-}
{-# language TypeInType #-}
{-# language TypeFamilies #-}

module Accordion.Types
  ( -- data types
    Nat(..)
  , Fin(..)
  , Vec(..)
  , Set(..)
  , Optional(..)
    -- type families
  , Empty
  ) where

import Data.Kind (Type)

-- Natural Numbers
data Nat = Zero | Succ Nat
-- Finite Numbers
data Fin :: Nat -> Type where
  FinZero :: Fin ('Succ n)
  FinSucc :: Fin n -> Fin ('Succ n)
data Vec :: Nat -> Type -> Type where
  VecNil :: Vec 'Zero a
  VecCons :: a -> Vec n a -> Vec ('Succ n) a
data Set :: Nat -> Type where
  SetLeaf :: Bool -> Set Zero
  SetBranch :: Set n -> Set n -> Set ('Succ n)
data SetFin (n :: Nat) (f :: Fin n) where
  SetFinLeaf :: Bool -> SetFin n 'FinZero
  SetFinBranch :: SetFin n f -> SetFin n f -> SetFin ('Succ n) ('FinSucc f)
data Optional :: Bool -> Type -> Type where
  Absent :: Optional 'False a
  Present :: a -> Optional 'True a

type family Plus (m :: Nat) (n :: Nat) where
  Plus 'Zero n = n
  Plus ('Succ m) n = 'Succ (Plus m n)

-- data AntiFin (n :: Nat) (f :: Fin n) where

-- Construct an empty set of a given height.
type family Empty (h :: Nat) :: Set h where
  Empty 'Zero = SetLeaf 'False
  Empty ('Succ h) = SetBranch (Empty h) (Empty h)

type family Singleton (t :: Nat) (h :: Fin t) (v :: Vec h Bool) (x :: Set p) :: Set t where
  Singleton 'Zero p 'VecNil s = s
  Singleton ('Succ h) p ('VecCons 'False v) s =
    Singleton h ('Succ p) v (SetBranch s (Empty p))

-- Construct a singleton set. One leaf is true and all others
-- are false. Unfortunately, we cannot write this since
-- the constructive approach to addition makes things awful.
-- type family Singleton (h :: Nat) (p :: Nat) (v :: Vec h Bool) (x :: Set p) :: Set (Plus h p) where
--   Singleton 'Zero p 'VecNil s = s
--   Singleton ('Succ h) p ('VecCons 'False v) s =
--     Singleton h ('Succ p) v (SetBranch s (Empty p))
