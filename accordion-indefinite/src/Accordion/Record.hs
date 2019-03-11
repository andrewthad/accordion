{-# language DataKinds #-}
{-# language GADTs #-}
{-# language KindSignatures #-}
{-# language PolyKinds #-}
{-# language TypeFamilies #-}
{-# language TypeInType #-}

module Accordion.Record
  ( Record(..)
  , Tree(..)
  , singleton
  ) where

import Accordion.Types (Nat(..),Set(..),Vec(..),Optional(..))
import Accordion.Universe (Height,Interpret)
import Data.Kind (Type)

newtype Record :: Set Height -> Type where
  Record :: Tree Height s 'Zero 'VecNil -> Record s

data Tree (h :: Nat) (s :: Set h) (p :: Nat) (v :: Vec p Bool) where
  TreeLeaf ::
       Optional b (Interpret v)
    -> Tree 'Zero ('SetLeaf b) Height v
  TreeBranch ::
       Tree h sl ('Succ p) ('VecCons False v)
    -> Tree h sr ('Succ p) ('VecCons True v)
    -> Tree ('Succ h) ('SetBranch sl sr) p v

-- singleton :: Interpret v -> Record s

-- type family Singleton (h :: Fin Height) (v :: Vec h Bool) (x :: Set p) :: Set Height where
--   Singleton 'Zero p 'VecNil s = s
--   Singleton ('Succ h) p ('VecCons 'False v) s =
--     Singleton h ('Succ p) v (SetBranch s (Empty p))
