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
  , word16
  , word8
    -- optional
  , word64Opt
  , boolOpt
  , intOpt
  , word16Opt
  , word8Opt
  ) where

import Accordion.Json.Types (Encode(..),EncodeOptional(..))
import Data.Primitive.ByteArray.Offset (MutableByteArrayOffset(..))
import Data.Tuple.Types (DoublePair(..))
import Data.Word (Word8)
import Data.Char (ord)
import Data.Array.DoublePair (DoublePairVector)
import qualified Encoding.Word64 as Word64
import qualified Encoding.Word16 as Word16
import qualified Encoding.Word8 as Word8
import qualified Encoding.Int as Int
import qualified Encoding.Bool as Bool

import qualified Data.Index as Index
import qualified Data.Array.Indexed as V
import qualified Data.ByteArray.Builder.Small.Unsafe as BB

bool :: Encode Bool.Vector
bool = Bool.req

boolOpt :: EncodeOptional Bool.Vector
boolOpt = Bool.opt

word64 :: Encode Word64.Vector
word64 = Word64.req

word64Opt :: EncodeOptional Word64.Vector
word64Opt = Word64.opt

word16 :: Encode Word16.Vector
word16 = Word16.req

word8 :: Encode Word8.Vector
word8 = Word8.req

word16Opt :: EncodeOptional Word16.Vector
word16Opt = Word16.opt

word8Opt :: EncodeOptional Word8.Vector
word8Opt = Word8.opt

int :: Encode Int.Vector
int = Int.req

intOpt :: EncodeOptional Int.Vector
intOpt = Int.opt

-- | Encode a @DoublePair@ as a two-length array. This can be useful
-- for converting longitude-latitude pairs to the GeoJSON format expected
-- by Elasticsearch.
-- doublePair :: Encode DoublePairVector
-- doublePair = Encode $ \bufs offs n xs -> Index.ascendM
--   ( \(Index.Index lt ix) -> do
--     buf0 <- V.read lt bufs ix
--     off0 <- Int.read lt offs ix
--     let !(DoublePair x y) = DoublePair.index lt xs ix
--     MutableByteArrayOffset buf1 off1 <-
--       BB.pasteGrowST (encodeOneDoublePair x y) (MutableByteArrayOffset buf0 off0)
--     V.write lt bufs ix buf1
--     Int.write lt offs ix off1
--   ) n

-- encodeOneDoublePair :: Double -> Double -> BB.Builder 67
-- encodeOneDoublePair x y =
--   BB.word8 (c2w '[')
--   `BB.append`
--   BB.doubleDec x
--   `BB.append`
--   BB.word8 (c2w ',')
--   `BB.append`
--   BB.doubleDec y
--   `BB.append`
--   BB.word8 (c2w ']')
-- 
-- c2w :: Char -> Word8
-- c2w = fromIntegral . ord
