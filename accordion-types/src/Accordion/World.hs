{-# language GADTs #-}
{-# language PolyKinds #-}
{-# language DataKinds #-}
{-# language RankNTypes #-}
{-# language TypeFamilies #-}
{-# language TypeOperators #-}

module Accordion.World
  ( World(..)
  , SingWorld(..)
  , SingMultiplicity(..)
  , Primitive(..)
  , Multiplicity(..)
  , SingPrimitive(..)
  , GroundWorld
  , GroundPrimitive
  , VectorizePrimitive
  , VectorizeWorld
  , appendVectors
  , singletonVector
    -- Hacks
  , FlipVector(..)
  ) where

import Data.Kind (Type)
import Data.Primitive (PrimArray,Array)
import Data.Word (Word8)
import Data.Functor.Identity (Identity(..))
import Data.Array.Indexed (Vector)
import GHC.TypeNats (type (+))

import qualified GHC.TypeNats as GHC
import qualified Data.Array.Indexed as V

data World = World Multiplicity Primitive

data Multiplicity
  = One
  | Optional
  | Many

data Primitive
  = Int
  | Bool
  | Char

data SingPrimitive :: Primitive -> Type where
  SingInt :: SingPrimitive 'Int
  SingBool :: SingPrimitive 'Bool
  SingChar :: SingPrimitive 'Char

data SingMultiplicity :: Multiplicity -> Type where
  SingOne :: SingMultiplicity 'One
  SingOptional :: SingMultiplicity 'Optional
  SingMany :: SingMultiplicity 'Many

data SingWorld :: World -> Type where
  SingWorld :: SingMultiplicity m -> SingPrimitive p -> SingWorld ('World m p)

type family GroundWorld (w :: World) :: Type where
  GroundWorld ('World m p) = GroundMultiplicity m (GroundPrimitive p)

type family GroundMultiplicity (m :: Multiplicity) :: Type -> Type where
  GroundMultiplicity 'One = Identity
  GroundMultiplicity 'Many = PrimArray
  GroundMultiplicity 'Optional = Maybe

type family GroundPrimitive (p :: Primitive) :: Type where
  GroundPrimitive 'Int = Int
  GroundPrimitive 'Bool = Bool
  GroundPrimitive 'Char = Char

type family VectorizeWorld (w :: World) :: GHC.Nat -> Type where
  VectorizeWorld ('World 'One p) = FlipVector (GroundPrimitive p)
  VectorizeWorld ('World 'Optional p) = FlipVector (GroundPrimitive p)
  VectorizeWorld ('World 'Many p) = FlipVector (Array (GroundPrimitive p))

type family VectorizePrimitive (p :: Primitive) :: Type where
  VectorizePrimitive 'Int = PrimArray Int
  VectorizePrimitive 'Bool = PrimArray Word8
  VectorizePrimitive 'Char = PrimArray Char

newtype FlipVector a n = FlipVector (Vector n a)

appendVectors ::
     SingWorld w
  -> VectorizeWorld w n
  -> VectorizeWorld w m
  -> VectorizeWorld w (n + m)
appendVectors (SingWorld SingOne SingInt) (FlipVector x) (FlipVector y) =
  FlipVector (V.append x y)

singletonVector ::
     SingWorld w
  -> GroundWorld w
  -> VectorizeWorld w 1
singletonVector (SingWorld SingOne SingInt) (Identity x) = FlipVector (V.singleton x)

-- type family VectorElem (w :: World (k :: Type)) :: Type where
--   VectorElem 'Int = Int
--   VectorElem 'Bool = Bool
--   VectorElem 'Char = Char
--   VectorElem ('Other x) = x
-- 
-- type family Vectorized (k :: Type) (w :: World k) :: Type where
--   Vectorized k 'Int = PrimArray Int
--   Vectorized k 'Bool = Array Bool
--   Vectorized k 'Char = PrimArray Char
--   Vectorized k ('Other x) = Array (f x)
-- 
-- vectorizedEquality :: SingWorld f w -> 
