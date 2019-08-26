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

module Accordion.Json.Types
  ( Encode(..)
  , EncodeOptional(..)
  ) where

import Control.Monad.ST (ST)
import Data.Array.Indexed (MutableVector,Vector)
import Data.Array.Bool (BoolVector)
import Data.Primitive (MutableByteArray,ByteArray)
import Data.Array.Int (MutableIntVector)
import qualified Data.Arithmetic.Types as Arithmetic

newtype Encode a = Encode (forall s n.
     MutableVector s n (MutableByteArray s)
  -> MutableIntVector s n -- indexes into byte arrays (unchecked)
  -> Arithmetic.Nat n
  -> a n
  -> ST s ()
  )

newtype EncodeOptional a = EncodeOptional (forall s n.
     MutableVector s n (MutableByteArray s)
  -> MutableIntVector s n -- indexes into byte arrays (unchecked)
  -> BoolVector n -- whether or not each element is valid
  -> Arithmetic.Nat n
  -> a n
  -> ST s ()
  )
