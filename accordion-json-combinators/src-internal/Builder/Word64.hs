module Builder.Word64
  ( builder
  ) where

import Data.Word (Word64)
import qualified Data.ByteArray.Builder as B

builder :: Word64 -> B.Builder 
builder = B.word64Dec
