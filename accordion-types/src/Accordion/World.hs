{-# language GADTs #-}
{-# language PolyKinds #-}
{-# language DataKinds #-}
{-# language RankNTypes #-}
{-# language TypeFamilies #-}

module Accordion.World
  ( World(..)
  , SingWorld(..)
  , Primitive(..)
  , SingPrimitive(..)
  , GroundPrimitive
  , VectorizePrimitive
  ) where

import Data.Kind (Type)
import Data.Primitive (PrimArray)
import Data.Word (Word8)

data World a
  = Primitive Primitive
  | Other a

data Primitive
  = Int
  | Bool
  | Char

data SingPrimitive :: Primitive -> Type where
  SingInt :: SingPrimitive 'Int
  SingBool :: SingPrimitive 'Bool
  SingChar :: SingPrimitive 'Char

data SingWorld :: forall (k :: Type). (k -> Type) -> World k -> Type where
  SingPrimitive :: SingPrimitive p -> SingWorld s ('Primitive p)
  SingOther :: s e -> SingWorld s ('Other e)

type family GroundPrimitive (p :: Primitive) :: Type where
  GroundPrimitive 'Int = Int
  GroundPrimitive 'Bool = Bool
  GroundPrimitive 'Char = Char

type family VectorizePrimitive (p :: Primitive) :: Type where
  VectorizePrimitive 'Int = PrimArray Int
  VectorizePrimitive 'Bool = PrimArray Word8
  VectorizePrimitive 'Char = PrimArray Char

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
