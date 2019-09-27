{-# language DataKinds #-}
{-# language ExplicitNamespaces #-}
{-# language TypeOperators #-}
{-# language TypeApplications #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}

module Indefinite
  ( V.Vector
  , V.MutableVector
  , V.substitute
  , V.equals
  , V.write
  , V.read
  , V.index
  , V.uninitialized
  , V.unsafeFreeze
  , V.set
  , append
  , singleton
  , rightPad
  , leftPad
  , replicate
  , initialized
  ) where

import Prelude hiding (replicate)

import Control.Monad.ST (ST,runST)
import Arithmetic.Types (Nat)
import Element (T)
import GHC.TypeNats (type (+))

import qualified Arithmetic.Equal as Equal
import qualified Arithmetic.Lt as Lt
import qualified Arithmetic.Lte as Lte
import qualified Arithmetic.Nat as Nat
import qualified Arithmetic.Plus as Plus
import qualified Vector.Unboxed as V

append :: forall m n. Nat m -> Nat n -> V.Vector m -> V.Vector n -> V.Vector (m + n)
append xlen ylen x y = runST $ do
  r <- V.uninitialized (Nat.plus xlen ylen)
  V.copy
    (Lte.incrementL @m (Lte.zero @n))
    Lte.reflexive
    r Nat.zero x Nat.zero xlen
  V.copy
    Lte.reflexive
    Lte.reflexive
    r xlen y Nat.zero ylen
  V.unsafeFreeze r

singleton :: T -> V.Vector 1
singleton x = runST $ do
  arr <- V.uninitialized Nat.one
  V.write Lt.zero arr Nat.zero x
  V.unsafeFreeze arr

rightPad :: forall m n.
     Nat n
  -> Nat m
  -> V.Vector n
  -> V.Vector (n + m)
rightPad n m v = runST $ do
  marr <- V.uninitialized (Nat.plus n m)
  V.copy
    (Lte.incrementL @n (Lte.zero @m))
    Lte.reflexive
    marr Nat.zero v Nat.zero n
  V.unsafeFreeze marr

leftPad :: forall m n.
     Nat n
  -> Nat m
  -> V.Vector n
  -> V.Vector (m + n)
leftPad n m v = runST $ do
  let totalLen :: Nat (m + n)
      totalLen = Nat.plus m n
  marr <- V.uninitialized totalLen
  V.copy
    Lte.reflexive
    Lte.reflexive
    marr m v Nat.zero n
  V.unsafeFreeze marr

replicate :: forall n. Nat n -> T -> V.Vector n
replicate n t = runST $ do
  m <- V.uninitialized n
  V.set Lte.reflexive m Nat.zero n t
  V.unsafeFreeze m

initialized :: forall n s. Nat n -> T -> ST s (V.MutableVector s n)
initialized n t = do
  m <- V.uninitialized n
  V.set Lte.reflexive m Nat.zero n t
  -- V.set (Lt.plus @n Lt.zero) m Nat.zero n t
  pure m
