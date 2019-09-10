module Builder.Word8
  ( builder
  ) where

import Data.Word (Word8)
import qualified Data.ByteArray.Builder as B

builder :: Word8 -> B.Builder 
builder = B.word8Dec
