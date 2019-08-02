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
  ( word64
  , int
  ) where

import Accordion.Json.Types (Encode(..))
import Data.Word (Word64)
import Data.Int (Int64)
import Data.Primitive.ByteArray.Offset (MutableByteArrayOffset(..))
import Data.Array.Indexed (Vector)
import Accordion.World (FlipVector(..))
import qualified Data.Arithmetic.Types as Arithmetic
import qualified GHC.TypeNats as GHC
import qualified Data.Index as Index
import qualified Data.Array.Indexed as V
import qualified Data.ByteArray.Builder.Small.Unsafe as BB
import qualified Data.ByteArray.Builder.Small as BBS

word64 :: Encode (FlipVector Word64)
word64 = Encode $ \bufs offs n (FlipVector xs) -> Index.ascendM
  ( \(Index.Index lt ix) -> do
    buf0 <- V.read lt bufs ix
    off0 <- V.read lt offs ix
    let !w = V.index lt xs ix
    MutableByteArrayOffset buf1 off1 <-
      BB.pasteGrowST (BB.word64Dec w) (MutableByteArrayOffset buf0 off0)
    V.write lt bufs ix buf1
    V.write lt offs ix off1
  ) n

int :: Encode (FlipVector Int)
int = Encode $ \bufs offs n (FlipVector xs) -> Index.ascendM
  ( \(Index.Index lt ix) -> do
    buf0 <- V.read lt bufs ix
    off0 <- V.read lt offs ix
    let !w = V.index lt xs ix
    MutableByteArrayOffset buf1 off1 <-
      BB.pasteGrowST (BB.int64Dec (fromIntegral w)) (MutableByteArrayOffset buf0 off0)
    V.write lt bufs ix buf1
    V.write lt offs ix off1
  ) n
