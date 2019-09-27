{-# language BangPatterns #-}
{-# language DataKinds #-}
{-# language KindSignatures #-}
{-# language LambdaCase #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}

module Deferred
  ( Deferred(Absent)
  , write
  , freeze
  ) where

import Element (T)
import Control.Monad.ST (ST)
import Arithmetic.Types (Nat)
import Data.Index (Index(..))
import Data.STRef (STRef,writeSTRef,readSTRef)
import qualified GHC.TypeNats as GHC
import qualified Arithmetic.Nat as Nat
import qualified Arithmetic.Lt as Lt
import qualified Arithmetic.Lte as Lte
import qualified Element as E
import qualified Vector.Unboxed as V
import qualified World.Bool as Bool

data Deferred s (n :: GHC.Nat)
  = Present
      !(V.MutableVector s n)
      !(Bool.MutableVector s n)
  | Absent

freeze ::
     Deferred s n
  -> ST s (Maybe (V.Vector n, Bool.Vector n))
freeze = \case
  Absent -> pure Nothing
  Present vs bs -> do
    vs' <- V.unsafeFreeze vs
    bs' <- Bool.unsafeFreeze bs
    pure (Just (vs',bs'))

write :: 
     STRef s (Deferred s n)
  -> Nat n
  -> Index n
  -> T
  -> ST s ()
write !ref !n (Index lt off) !v = do
  (vs, bs) <- ensure ref n
  Bool.write lt bs off True
  V.write lt vs off v

ensure ::
     STRef s (Deferred s n)
  -> Nat n
  -> ST s (V.MutableVector s n, Bool.MutableVector s n)
ensure !ref !n = readSTRef ref >>= \case
  Present x y -> pure (x,y)
  Absent -> do
    !vs <- initialized n E.def
    !bs <- Bool.initialized n False
    let !x = Present vs bs
    writeSTRef ref x
    pure (vs,bs)

initialized :: forall n s. Nat n -> T -> ST s (V.MutableVector s n)
initialized n t = do
  m <- V.uninitialized n
  V.set Lte.reflexive m Nat.zero n t
  pure m
