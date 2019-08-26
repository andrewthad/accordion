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
import Data.Text.Short (ShortText)
import Control.Monad.ST (runST)
import GHC.TypeNats (type (+))

import qualified Data.Tuple.Types as Tuple
import qualified GHC.TypeNats as GHC
import qualified Data.Array.Indexed as V
import qualified Data.Array.Int as Int
import qualified Data.Array.Bool as Bool
import qualified Data.Array.Char as Char
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
  | Word16
  | Word32
  | Word64

data SingWorld :: World -> Type where
  SingBool :: SingWorld 'Bool
  SingChar :: SingWorld 'Char
  SingDoublePair :: SingWorld 'DoublePair
  SingInt :: SingWorld 'Int
  SingInt64 :: SingWorld 'Int64
  SingText :: SingWorld 'Text
  SingTexts :: SingWorld 'Texts
  SingWord16 :: SingWorld 'Word16
  SingWord32 :: SingWorld 'Word32
  SingWord64 :: SingWorld 'Word64

type family GroundWorld (p :: World) :: Type where
  GroundWorld 'Bool = Bool
  GroundWorld 'Char = Char
  GroundWorld 'DoublePair = Tuple.DoublePair
  GroundWorld 'Int = Int
  GroundWorld 'Int64 = Int64
  GroundWorld 'Text = ShortText
  GroundWorld 'Texts = [ShortText]
  GroundWorld 'Word16 = Word16
  GroundWorld 'Word32 = Word32
  GroundWorld 'Word64 = Word64

type family VectorizeWorld (w :: World) :: GHC.Nat -> Type where
  VectorizeWorld 'Int = IntVector
  VectorizeWorld 'Bool = BoolVector
  VectorizeWorld 'Char = CharVector
  VectorizeWorld 'Word16 = Word16Vector

appendVectors ::
     SingWorld w
  -> VectorizeWorld w n
  -> VectorizeWorld w m
  -> VectorizeWorld w (n + m)
appendVectors SingInt = Int.append
appendVectors SingBool = Bool.append
appendVectors SingChar = Char.append

singletonVector ::
     SingWorld w
  -> GroundWorld w
  -> VectorizeWorld w 1
singletonVector SingInt x = Int.singleton x
singletonVector SingChar x = Char.singleton x
singletonVector SingBool x = Bool.singleton x

rightPadVector :: forall w m n.
     SingWorld w
  -> Arithmetic.Nat m
  -> VectorizeWorld w n
  -> VectorizeWorld w (n + m)
rightPadVector SingInt = \m v -> runST $ do
  let n = Int.length v
  marr <- Int.new (Nat.plus n m)
  Int.copy
    ( Lt.substituteR (Equal.symmetric (Plus.associative @n @m @1))
    $ Lt.plus @n (Lt.zero @m)
    )
    (Lt.plus @n (Lt.zero @0))
    marr Nat.zero v Nat.zero n
  Int.unsafeFreeze marr

leftPadVector :: forall w m n.
     SingWorld w
  -> Arithmetic.Nat m
  -> VectorizeWorld w n
  -> VectorizeWorld w (m + n)
leftPadVector SingInt = \m v -> runST $ do
  let n = Int.length v
  let totalLen :: Arithmetic.Nat (m + n)
      totalLen = Nat.plus m n
  marr <- Int.new totalLen
  Int.copy
    (Lt.plus @(m + n) (Lt.zero @0))
    (Lt.plus @n (Lt.zero @0))
    marr m v Nat.zero n
  Int.unsafeFreeze marr

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
