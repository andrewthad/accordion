{-# language BangPatterns #-}
{-# language GADTs #-}
{-# language ScopedTypeVariables #-}
{-# language PolyKinds #-}
{-# language DataKinds #-}
{-# language RankNTypes #-}
{-# language TypeFamilies #-}
{-# language TypeApplications #-}
{-# language TypeOperators #-}

module Accordion.World
  ( World(..)
  , SingWorld(..)
  , GroundWorld
  , VectorizeWorld
  , appendVectors
  , eqVectors
  , substituteVector
  , singletonVector
  , rightPadVector
  , leftPadVector
  ) where

import Data.Kind (Type)
import Data.Primitive (PrimArray,Array)
import Data.Word (Word8,Word16,Word32,Word64)
import Data.Int (Int64)
import Data.Functor.Identity (Identity(..))
import Data.Array.Indexed (Vector)
import Data.Array.Int (IntVector)
import Data.Array.Char (CharVector)
import Data.Array.Bool (BoolVector)
import Data.Array.Word16 (Word16Vector)
import Data.Array.Word64 (Word64Vector)
import Data.Text.Short (ShortText)
import Control.Monad.ST (runST)
import GHC.TypeNats (type (+))
import Data.Arithmetic.Types ((:=:),Nat)
import Data.WideWord.Word128 (Word128)

import qualified Data.Tuple.Types as Tuple
import qualified GHC.TypeNats as GHC
import qualified Data.Array.Indexed as V
import qualified World.Int as Int
import qualified World.Char as Char
import qualified World.Word8 as Word8
import qualified World.Bool as Bool
import qualified World.Word16 as Word16
import qualified World.Word32 as Word32
import qualified World.Word64 as Word64
import qualified World.Word128 as Word128
import qualified Data.Arithmetic.Types as Arithmetic
import qualified Data.Arithmetic.Nat as Nat
import qualified Data.Arithmetic.Lt as Lt
import qualified Data.Arithmetic.Plus as Plus
import qualified Data.Arithmetic.Equal as Equal

data World
  = Bool
  | Char
  | DoublePair
  | Int
  | Int64
  | Text
  | Texts
  | Word8
  | Word16
  | Word32
  | Word64
  | Word128

data SingWorld :: World -> Type where
  SingBool :: SingWorld 'Bool
  SingChar :: SingWorld 'Char
  SingDoublePair :: SingWorld 'DoublePair
  SingInt :: SingWorld 'Int
  SingInt64 :: SingWorld 'Int64
  SingText :: SingWorld 'Text
  SingTexts :: SingWorld 'Texts
  SingWord8 :: SingWorld 'Word8
  SingWord16 :: SingWorld 'Word16
  SingWord32 :: SingWorld 'Word32
  SingWord64 :: SingWorld 'Word64
  SingWord128 :: SingWorld 'Word128

type family GroundWorld (p :: World) :: Type where
  GroundWorld 'Bool = Bool
  GroundWorld 'Char = Char
  GroundWorld 'DoublePair = Tuple.DoublePair
  GroundWorld 'Int = Int
  GroundWorld 'Int64 = Int64
  GroundWorld 'Text = ShortText
  GroundWorld 'Texts = [ShortText]
  GroundWorld 'Word8 = Word8
  GroundWorld 'Word16 = Word16
  GroundWorld 'Word32 = Word32
  GroundWorld 'Word64 = Word64
  GroundWorld 'Word128 = Word128

type family VectorizeWorld (w :: World) :: GHC.Nat -> Type where
  VectorizeWorld 'Int = Int.Vector
  VectorizeWorld 'Bool = Bool.Vector
  VectorizeWorld 'Char = Char.Vector
  VectorizeWorld 'Word8 = Word8.Vector
  VectorizeWorld 'Word16 = Word16.Vector
  VectorizeWorld 'Word32 = Word32.Vector
  VectorizeWorld 'Word64 = Word64.Vector
  VectorizeWorld 'Word128 = Word128.Vector

substituteVector ::
     SingWorld w
  -> n :=: m
  -> VectorizeWorld w n
  -> VectorizeWorld w m
substituteVector !w !e !v = case w of
  SingInt -> Int.substitute e v
  SingChar -> Char.substitute e v
  SingWord8 -> Word8.substitute e v
  SingWord16 -> Word16.substitute e v
  SingWord64 -> Word64.substitute e v

eqVectors ::
     SingWorld w
  -> Nat n
  -> VectorizeWorld w n
  -> VectorizeWorld w n
  -> Bool
eqVectors !w !n !a !b = case w of
  SingInt -> Int.equals n a b
  SingBool -> Bool.equals n a b
  SingChar -> Char.equals n a b
  SingWord8 -> Word8.equals n a b
  SingWord16 -> Word16.equals n a b
  SingWord64 -> Word64.equals n a b
  SingWord128 -> Word128.equals n a b

appendVectors ::
     SingWorld w
  -> Nat n
  -> Nat m
  -> VectorizeWorld w n
  -> VectorizeWorld w m
  -> VectorizeWorld w (n + m)
appendVectors !w !n !m !a !b = case w of
  SingInt -> Int.append n m a b
  SingBool -> Bool.append n m a b
  SingChar -> Char.append n m a b
  SingWord8 -> Word8.append n m a b
  SingWord16 -> Word16.append n m a b
  SingWord64 -> Word64.append n m a b
  SingWord128 -> Word128.append n m a b

singletonVector ::
     SingWorld w
  -> GroundWorld w
  -> VectorizeWorld w 1
singletonVector w !x = case w of
  SingInt -> Int.singleton x
  SingBool -> Bool.singleton x
  SingChar -> Char.singleton x
  SingWord8 -> Word8.singleton x
  SingWord16 -> Word16.singleton x
  SingWord64 -> Word64.singleton x
  SingWord128 -> Word128.singleton x

rightPadVector :: forall w m n.
     SingWorld w
  -> Nat n
  -> Nat m
  -> VectorizeWorld w n
  -> VectorizeWorld w (n + m)
rightPadVector !w !n !m !v = case w of
  SingInt -> Int.rightPad n m v
  SingChar -> Char.rightPad n m v
  SingBool -> Bool.rightPad n m v
  SingWord8 -> Word8.rightPad n m v
  SingWord16 -> Word16.rightPad n m v
  SingWord64 -> Word64.rightPad n m v
  SingWord128 -> Word128.rightPad n m v

leftPadVector :: forall w m n.
     SingWorld w
  -> Nat n
  -> Nat m
  -> VectorizeWorld w n
  -> VectorizeWorld w (m + n)
leftPadVector !w !n !m !v = case w of
  SingInt -> Int.leftPad n m v
  SingChar -> Char.leftPad n m v
  SingBool -> Bool.leftPad n m v
  SingWord8 -> Word8.leftPad n m v
  SingWord16 -> Word16.leftPad n m v
  SingWord64 -> Word64.leftPad n m v
  SingWord128 -> Word128.leftPad n m v

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
