{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language BangPatterns #-}

module Accordion.Vector
  ( -- * Eq
    intEq
  , charEq
  , boolEq
  , liftedEq
    -- * Filter
  , intFilter
  , charFilter
  , boolFilter
  , liftedFilter
  ) where

import Data.Primitive (Prim,MutablePrimArray,PrimArray,Array)
import Control.Monad.ST (ST,runST)
import Data.Word (Word8)
import Data.Bits ((.&.))
import qualified Data.Primitive as PM

intEq :: Int -> PrimArray Int -> MutablePrimArray s Word8 -> Int -> Int -> ST s ()
intEq !a !b !c !d !e = generalEq a b c d e

charEq :: Char -> PrimArray Char -> MutablePrimArray s Word8 -> Int -> Int -> ST s ()
charEq !a !b !c !d !e = generalEq a b c d e

boolEq :: Bool -> PrimArray Word8 -> MutablePrimArray s Word8 -> Int -> Int -> ST s ()
boolEq !a !b !c !d !e = generalEq (fromIntegral @Int @Word8 (fromEnum a)) b c d e

liftedEq :: (a -> a -> Bool) -> a -> Array a -> MutablePrimArray s Word8 -> Int -> Int -> ST s ()
liftedEq f x xs r = go where
  go off len = if len > 0
    then do
      let y = PM.indexArray xs off
      old :: Word8 <- PM.readPrimArray r off
      PM.writePrimArray r off (old .&. fromIntegral @Int @Word8 (fromEnum (f x y)))
      go (off + 1) (len - 1)
    else pure ()

generalEq :: (Prim a, Eq a)
  => a -> PrimArray a -> MutablePrimArray s Word8 -> Int -> Int -> ST s ()
{-# inline generalEq #-}
generalEq x xs r = go where
  go off len = if len > 0
    then do
      let y = PM.indexPrimArray xs off
      old :: Word8 <- PM.readPrimArray r off
      PM.writePrimArray r off (old .&. fromIntegral @Int @Word8 (fromEnum (x == y)))
      go (off + 1) (len - 1)
    else pure ()

intFilter :: PrimArray Int -> PrimArray Word8 -> Int -> PrimArray Int
intFilter !a !b !c = generalFilter a b c

charFilter :: PrimArray Char -> PrimArray Word8 -> Int -> PrimArray Char
charFilter !a !b !c = generalFilter a b c

boolFilter :: PrimArray Word8 -> PrimArray Word8 -> Int -> PrimArray Word8
boolFilter !a !b !c = generalFilter a b c

generalFilter :: Prim a
  => PrimArray a -> PrimArray Word8 -> Int -> PrimArray a
generalFilter !xs !keeps !total = runST $ do
  dst <- PM.newPrimArray total
  let go !off !offDst = if offDst >= 0
        then if PM.indexPrimArray keeps off == 0
          then do
            go (off - 1) offDst
          else do
            PM.writePrimArray dst offDst (PM.indexPrimArray xs off)
            go (off - 1) (offDst - 1)
        else pure ()
  go (PM.sizeofPrimArray xs - 1) (total - 1)
  PM.unsafeFreezePrimArray dst

liftedFilter :: Array a -> PrimArray Word8 -> Int -> Array a
liftedFilter !xs !keeps !total = runST $ do
  dst <- PM.newArray total errorThunk
  let go !off !offDst = if offDst >= 0
        then if PM.indexPrimArray keeps off == 0
          then do
            go (off - 1) offDst
          else do
            PM.writeArray dst offDst (PM.indexArray xs off)
            go (off - 1) (offDst - 1)
        else pure ()
  go (PM.sizeofArray xs - 1) (total - 1)
  PM.unsafeFreezeArray dst

errorThunk :: a
{-# noinline errorThunk #-}
errorThunk = error "Accordion.Vector: errorThunk"
