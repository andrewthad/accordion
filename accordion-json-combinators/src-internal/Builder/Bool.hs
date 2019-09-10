{-# language DataKinds #-}

module Builder.Bool
  ( builder
  ) where

import Data.Word (Word8)
import Data.Char (ord)
import qualified Arithmetic.Nat as Nat
import qualified Data.ByteArray.Builder as B
import qualified Data.ByteArray.Builder.Bounded as BU

builder :: Bool -> B.Builder
builder b = case b of
  True -> B.fromBounded Nat.constant encodeTrue
  False -> B.fromBounded Nat.constant encodeFalse

encodeTrue :: BU.Builder 4
encodeTrue = 
  BU.word8 (c2w 't')
  `BU.append`
  BU.word8 (c2w 'r')
  `BU.append`
  BU.word8 (c2w 'u')
  `BU.append`
  BU.word8 (c2w 'e')

encodeFalse :: BU.Builder 5
encodeFalse = 
  BU.word8 (c2w 'f')
  `BU.append`
  BU.word8 (c2w 'a')
  `BU.append`
  BU.word8 (c2w 'l')
  `BU.append`
  BU.word8 (c2w 's')
  `BU.append`
  BU.word8 (c2w 'e')

c2w :: Char -> Word8
c2w = fromIntegral . ord
