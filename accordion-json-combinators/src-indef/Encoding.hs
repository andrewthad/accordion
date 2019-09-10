{-# language BangPatterns #-}

module Encoding
  ( V.Vector
  , req
  , opt
  ) where

import Builder (builder)
import Encode (Encode(..),EncodeOptional(..))
import Data.Primitive.ByteArray.Offset (MutableByteArrayOffset(..))
import qualified Data.ByteArray.Builder as BB
import qualified Data.Index as Index
import qualified Vector.Unboxed.Int as Int
import qualified Vector.Unboxed.Bool as Bool
import qualified Data.Array.Indexed as Boxed
import qualified Vector.Unboxed as V

req :: Encode V.Vector
req = Encode $ \bufs offs n xs -> Index.ascendM
  ( \(Index.Index lt ix) -> do
    buf0 <- Boxed.read lt bufs ix
    off0 <- Int.read lt offs ix
    let !w = V.index lt xs ix
    -- We actually do not need to make this loop. All of the integer
    -- pastes have a known upper bound on size. Reconsider this later.
    MutableByteArrayOffset buf1 off1 <-
      BB.pasteGrowST 64 (builder w) (MutableByteArrayOffset buf0 off0)
    Boxed.write lt bufs ix buf1
    Int.write lt offs ix off1
  ) n

opt :: EncodeOptional V.Vector
opt = EncodeOptional $ \bufs offs bs n xs -> Index.ascendM
  ( \(Index.Index lt ix) -> if Bool.index lt bs ix
    then do
      buf0 <- Boxed.read lt bufs ix
      off0 <- Int.read lt offs ix
      let !w = V.index lt xs ix
      -- We actually do not need to make this loop. All of the integer
      -- pastes have a known upper bound on size. Reconsider this later.
      MutableByteArrayOffset buf1 off1 <-
        BB.pasteGrowST 64 (builder w) (MutableByteArrayOffset buf0 off0)
      Boxed.write lt bufs ix buf1
      Int.write lt offs ix off1
    else pure ()
  ) n
