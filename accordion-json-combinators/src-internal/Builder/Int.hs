module Builder.Int
  ( builder
  ) where

import qualified Data.ByteArray.Builder as B

builder :: Int -> B.Builder 
builder = B.intDec


