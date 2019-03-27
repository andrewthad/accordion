{-# language PolyKinds #-}
{-# language DataKinds #-}
{-# language RankNTypes #-}
{-# language GADTs #-}

module Playground where

import Topaz.Rec
import Vector.Types
import Vector.Unboxed (UnboxedVector)
import Data.Kind (Type)
import GHC.TypeLits (Nat)

data Nest :: forall (k :: Type) (j :: Type). (Nat -> k -> Type) -> (Nat -> j -> Type) -> [k] -> [j] -> Type where
  Nest :: Length n
       -> Length m
       -> UnboxedVector n (Index m)
       -> Rec (f n) ks
       -> Rec (g m) js
       -> Nest f g ks js



