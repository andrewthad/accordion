{-# language BangPatterns #-}
{-# language DataKinds #-}
{-# language GADTs #-}
{-# language KindSignatures #-}
{-# language PolyKinds #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language TypeInType #-}
{-# language TypeOperators #-}

module Accordion.Json.Encode
  ( -- required
    word64
  , int
  , bool
  , doublePair
  , word16
    -- optional
  , boolOpt
  , intOpt
  , word16Opt
  ) where

import Accordion.Json.Types (Encode(..),EncodeOptional(..))
import Data.Primitive.ByteArray.Offset (MutableByteArrayOffset(..))
import Data.Tuple.Types (DoublePair(..))
import Data.Word (Word8)
import Data.Char (ord)
import Data.Array.Int (IntVector)
import Data.Array.Bool (BoolVector)
import Data.Array.DoublePair (DoublePairVector)
import Data.Array.Word64 (Word64Vector)
import Data.Array.Word16 (Word16Vector)
import qualified Data.Array.Word64 as Word64
import qualified Data.Array.Word16 as Word16
import qualified Data.Array.DoublePair as DoublePair
import qualified Data.Array.Bool as Bool
import qualified Data.Array.Int as Int
import qualified Data.Index as Index
import qualified Data.Array.Indexed as V
import qualified Data.ByteArray.Builder.Small.Unsafe as BB

bool :: Encode BoolVector
bool = Encode $ \bufs offs n xs -> Index.ascendM
  ( \(Index.Index lt ix) -> do
    buf0 <- V.read lt bufs ix
    off0 <- Int.read lt offs ix
    MutableByteArrayOffset buf1 off1 <- case Bool.index lt xs ix of
      True -> BB.pasteGrowST encodeTrue (MutableByteArrayOffset buf0 off0)
      False -> BB.pasteGrowST encodeFalse (MutableByteArrayOffset buf0 off0)
    V.write lt bufs ix buf1
    Int.write lt offs ix off1
  ) n

boolOpt :: EncodeOptional BoolVector
boolOpt = EncodeOptional $ \bufs offs bs n xs -> Index.ascendM
  ( \(Index.Index lt ix) -> if Bool.index lt bs ix
    then do
      buf0 <- V.read lt bufs ix
      off0 <- Int.read lt offs ix
      MutableByteArrayOffset buf1 off1 <- case Bool.index lt xs ix of
        True -> BB.pasteGrowST encodeTrue (MutableByteArrayOffset buf0 off0)
        False -> BB.pasteGrowST encodeFalse (MutableByteArrayOffset buf0 off0)
      V.write lt bufs ix buf1
      Int.write lt offs ix off1
    else pure ()
  ) n

word64 :: Encode Word64Vector
word64 = Encode $ \bufs offs n xs -> Index.ascendM
  ( \(Index.Index lt ix) -> do
    buf0 <- V.read lt bufs ix
    off0 <- Int.read lt offs ix
    let !w = Word64.index lt xs ix
    MutableByteArrayOffset buf1 off1 <-
      BB.pasteGrowST (BB.word64Dec w) (MutableByteArrayOffset buf0 off0)
    V.write lt bufs ix buf1
    Int.write lt offs ix off1
  ) n

word16 :: Encode Word16Vector
word16 = Encode $ \bufs offs n xs -> Index.ascendM
  ( \(Index.Index lt ix) -> do
    buf0 <- V.read lt bufs ix
    off0 <- Int.read lt offs ix
    let !w = Word16.index lt xs ix
    MutableByteArrayOffset buf1 off1 <-
      BB.pasteGrowST (BB.word16Dec w) (MutableByteArrayOffset buf0 off0)
    V.write lt bufs ix buf1
    Int.write lt offs ix off1
  ) n

int :: Encode IntVector
int = Encode $ \bufs offs n xs -> Index.ascendM
  ( \(Index.Index lt ix) -> do
    buf0 <- V.read lt bufs ix
    off0 <- Int.read lt offs ix
    let !w = Int.index lt xs ix
    MutableByteArrayOffset buf1 off1 <-
      BB.pasteGrowST (BB.int64Dec (fromIntegral w)) (MutableByteArrayOffset buf0 off0)
    V.write lt bufs ix buf1
    Int.write lt offs ix off1
  ) n

word16Opt :: EncodeOptional Word16Vector
word16Opt = EncodeOptional $ \bufs offs bs n xs -> Index.ascendM
  ( \(Index.Index lt ix) -> if Bool.index lt bs ix
    then do
      buf0 <- V.read lt bufs ix
      off0 <- Int.read lt offs ix
      let !w = Word16.index lt xs ix
      MutableByteArrayOffset buf1 off1 <-
        BB.pasteGrowST (BB.word16Dec w) (MutableByteArrayOffset buf0 off0)
      V.write lt bufs ix buf1
      Int.write lt offs ix off1
    else pure ()
  ) n

intOpt :: EncodeOptional IntVector
intOpt = EncodeOptional $ \bufs offs bs n xs -> Index.ascendM
  ( \(Index.Index lt ix) -> if Bool.index lt bs ix
    then do
      buf0 <- V.read lt bufs ix
      off0 <- Int.read lt offs ix
      let !w = Int.index lt xs ix
      MutableByteArrayOffset buf1 off1 <-
        BB.pasteGrowST (BB.int64Dec (fromIntegral w)) (MutableByteArrayOffset buf0 off0)
      V.write lt bufs ix buf1
      Int.write lt offs ix off1
    else pure ()
  ) n

-- | Encode a @DoublePair@ as a two-length array. This can be useful
-- for converting longitude-latitude pairs to the GeoJSON format expected
-- by Elasticsearch.
doublePair :: Encode DoublePairVector
doublePair = Encode $ \bufs offs n xs -> Index.ascendM
  ( \(Index.Index lt ix) -> do
    buf0 <- V.read lt bufs ix
    off0 <- Int.read lt offs ix
    let !(DoublePair x y) = DoublePair.index lt xs ix
    MutableByteArrayOffset buf1 off1 <-
      BB.pasteGrowST (encodeOneDoublePair x y) (MutableByteArrayOffset buf0 off0)
    V.write lt bufs ix buf1
    Int.write lt offs ix off1
  ) n

encodeOneDoublePair :: Double -> Double -> BB.Builder 67
encodeOneDoublePair x y =
  BB.word8 (c2w '[')
  `BB.append`
  BB.doubleDec x
  `BB.append`
  BB.word8 (c2w ',')
  `BB.append`
  BB.doubleDec y
  `BB.append`
  BB.word8 (c2w ']')

encodeTrue :: BB.Builder 4
encodeTrue = 
  BB.word8 (c2w 't')
  `BB.append`
  BB.word8 (c2w 'r')
  `BB.append`
  BB.word8 (c2w 'u')
  `BB.append`
  BB.word8 (c2w 'e')

encodeFalse :: BB.Builder 5
encodeFalse = 
  BB.word8 (c2w 'f')
  `BB.append`
  BB.word8 (c2w 'a')
  `BB.append`
  BB.word8 (c2w 'l')
  `BB.append`
  BB.word8 (c2w 's')
  `BB.append`
  BB.word8 (c2w 'e')

c2w :: Char -> Word8
c2w = fromIntegral . ord
