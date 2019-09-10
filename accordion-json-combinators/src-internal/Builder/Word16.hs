module Builder.Word16
  ( builder
  ) where

import Data.Word (Word16)
import qualified Data.ByteArray.Builder as B

builder :: Word16 -> B.Builder 
builder = B.word16Dec
