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
  ) where

import Control.Monad.ST (ST)
import Data.Array.Indexed (MutableVector,Vector)
import Data.Primitive (MutableByteArray,ByteArray)
import qualified Data.Arithmetic.Types as Arithmetic

newtype Encode a = Encode (forall s n.
     MutableVector s n (MutableByteArray s)
  -> MutableVector s n Int -- indexes into byte arrays (unchecked)
  -> Arithmetic.Nat n
  -> a n
  -> ST s ()
  )
