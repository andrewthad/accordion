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

module Accordion.Internal
  ( Vectorized(..)
  ) where

import Data.Kind (Type)
import Accordion.Types (ApConst1(..),ApConst2(..))
import Control.Monad.ST (runST)
import GHC.TypeLits (type (+))

import qualified Accordion.Types as A
import qualified Accordion.World as A
import qualified Data.Primitive as PM
import qualified Accordion.Base.Signature as S
import qualified Data.Array.Indexed as V
import qualified GHC.TypeNats as GHC
import qualified Arithmetic.Types as Arithmetic
import qualified Arithmetic.Nat as Nat
import qualified Data.Index as Index

newtype Vectorized :: GHC.Nat -> A.Vec S.FieldHeight Bool -> Type where
  Vectorized :: A.VectorizeWorld (S.Represent (S.Interpret v)) n -> Vectorized n v

