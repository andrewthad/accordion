{-# language DataKinds #-}
{-# language GADTs #-}
{-# language KindSignatures #-}
{-# language PolyKinds #-}
{-# language RankNTypes #-}
{-# language TypeFamilies #-}
{-# language TypeInType #-}

module Accordion.Record
  ( -- Data types
    Record(..)
  , Field(..)
    -- Functions
  , singleton
  , leftUnion
    -- Type families
  , Singleton
  , Union
  ) where

import Accordion.Types (Nat(..),Vec(..),SetFin(..),Finger(..))
import Accordion.Types (Tree(..),Gte(GteEq),Shown(..),Omnitree(..))
import Accordion.Universe (Height,Interpret,interpretShow)
import Data.Kind (Type)
import qualified Accordion.Types as A

newtype Record :: SetFin Height 'Zero -> Type where
  Record :: Tree Height 'Zero Interpret s 'VecNil -> Record s

instance Show (Record s) where
  show (Record t) = showTree interpretShow t "empty"

-- TODO: At the least, we should probably have a general function
-- for pairing an Omnitree and a tree that lives in Accordion.Types.
-- TODO: Fix the missing case warnings. It is not actually possible
-- to take these paths, but we need to help GHC out with Void/absurd.
showTree ::
     Omnitree Height n (Shown (Vec Height Bool) Interpret) v
  -> Tree Height n Interpret s v
  -> String
  -> String
showTree (OmnitreeLeaf (Shown f)) (TreeLeaf x) s = f x ++ " :> " ++ s
showTree (OmnitreeBranch oml omr) (TreeBranch dl dr) s = showTree oml dl (showTree omr dr s)
showTree (OmnitreeBranch oml _) (TreeLeft dl) s = showTree oml dl s
showTree (OmnitreeBranch _ omr) (TreeRight dr) s = showTree omr dr s
showTree (OmnitreeBranch _ _) TreeEmpty s = s

newtype Field :: Vec Height Bool -> Type where
  Field :: forall (v :: Vec Height Bool). Finger Height v -> Field v

type Singleton (v :: Vec Height Bool) =
  A.Singleton Height Height v 'SetFinLeaf 'GteEq

type Union (rs :: SetFin Height 'Zero) (ss :: SetFin Height 'Zero) =
  A.Union Height 'Zero rs ss

singleton ::
     Field v
  -> Interpret v
  -> Record (Singleton v)
singleton (Field finger) value =
  Record (A.singleton A.SingGteEq finger finger value (A.TreeLeaf value))

leftUnion :: Record rs -> Record ss -> Record (Union rs ss)
leftUnion (Record xs) (Record ys) = Record (A.leftUnion xs ys)
