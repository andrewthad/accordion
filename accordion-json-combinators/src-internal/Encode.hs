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

module Encode
  ( Encode(..)
  , EncodeOptional(..)
  ) where

import Control.Monad.ST (ST)
import Data.Array.Indexed (MutableVector)
import Data.Primitive (MutableByteArray)
import qualified Vector.Unboxed.Int as Int
import qualified Vector.Unboxed.Bool as Bool
import qualified Data.Arithmetic.Types as Arithmetic

newtype Encode a = Encode (forall s n.
     MutableVector s n (MutableByteArray s)
  -> Int.MutableVector s n -- indexes into byte arrays (unchecked)
  -> Arithmetic.Nat n
  -> a n
  -> ST s ()
  )

newtype EncodeOptional a = EncodeOptional (forall s n.
     MutableVector s n (MutableByteArray s)
  -> Int.MutableVector s n -- indexes into byte arrays (unchecked)
  -> Bool.Vector n -- whether or not each element is valid
  -> Arithmetic.Nat n
  -> a n
  -> ST s ()
  )
