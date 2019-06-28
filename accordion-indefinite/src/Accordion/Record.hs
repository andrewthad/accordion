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
{-# language UndecidableInstances #-}

module Accordion.Record
  ( -- Data types
    Record(..)
  , Records
  , Value(..)
    -- Functions
  , empty
  , singleton
  , leftUnion
  , (&>)
  , (<.>)
    -- Vector
  , one
  , toStructures
  , filter
  , tabulate
    -- Type families
  , Singleton
  , Union
  , FromList
  ) where

import Prelude hiding (filter)

import Accordion.Types (Nat(..),Vec(..),SetFin(..),Finger(..))
import Accordion.Types (Tree(..),Gte(GteEq),Omnitree(..))
import Accordion.Types (omnibuild,omnifoldr)
import Accordion.Universe (Height,Interpret,Ground,Interpret)
import Accordion.Universe (SingField,Index,Field,Extra)
import Accordion.Universe (singHeight,showsPrecUniverse,interpret,index)
import Accordion.Universe (showIndexField,InterpretExtra,SingExtra,eqExtra)
import Accordion.Universe (Represent,represent,toInternal,fromInternal)
import Accordion.Universe (GroundWorld)
import Control.Monad.ST (ST,runST)
import Data.Functor.Classes (Show1,liftShowsPrec)
import Data.Functor.Identity (Identity(..))
import Data.Primitive (MutablePrimArray,PrimArray,Array)
import Data.Kind (Type)
import Data.Word (Word8)
import GHC.Show (showList__)
import Accordion.World (World,SingWorld)

import qualified Accordion.Types as A
import qualified Accordion.Vector as V
import qualified Data.Primitive as PM
import qualified Accordion.World as W

newtype Record :: (Type -> Type) -> SetFin Height 'Zero -> Type where
  Record :: Tree Height 'Zero (Interpreted f) s 'VecNil -> Record f s

data Records :: SetFin Height 'Zero -> Type where
  Records :: !Int -- Number of rows
          -> Tree Height 'Zero Vectorized s 'VecNil -- Columns
          -> Records s

instance Semigroup (Records rs) where
  Records n1 t1 <> Records n2 t2 = Records (n1 + n2) $ A.zip
    (\fng (Vectorized v1) (Vectorized v2) ->
      Vectorized (vectorizedAppend (represent (interpret fng)) v1 v2)
    )
    FingerNil
    t1
    t2

filter :: Record Identity rs -> Records ss -> Records ss
filter (Record r) (Records n s) =
  let selected :: PrimArray Word8
      selected = runST $ do
        (enabled :: MutablePrimArray s Word8) <- PM.newPrimArray n
        -- Start off by marking all rows as being preserved.
        PM.setPrimArray enabled 0 n (1 :: Word8)
        A.zipM_
          ( \finger (Interpreted (Identity x)) (Vectorized vec) ->
              let uni = interpret finger
               in vectorizedEquality (represent uni) (toInternal uni x) vec enabled 0 n
          ) FingerNil r s
        PM.unsafeFreezePrimArray enabled
      -- Compute total number of elements to keep in each vector.
      total = PM.foldlPrimArray' (\acc w -> acc + fromIntegral w) (0 :: Int) selected
      t = A.map
        (\finger (Vectorized vec) ->
          Vectorized
            ( vectorizedFilter
              (represent (interpret finger))
              vec selected total
            )
        ) FingerNil s
   in Records total t

data Value :: (Type -> Type) -> Field -> Type where
  Value :: SingField d -> f (Ground (Interpret (Index d))) -> Value f d

instance Show1 f => Show (Record f s) where
  -- Is the number in the showParen predicate supposed to
  -- be 7 or 8? Not sure.
  showsPrec p (Record t) = showParen (p > 7) (showsRecord t . showString "empty")

newtype Interpreted :: (Type -> Type) -> Vec Height Bool -> Type where
  Interpreted :: f (Ground (Interpret v)) -> Interpreted f v

newtype Vectorized :: Vec Height Bool -> Type where
  Vectorized :: Vectorize (Represent (Interpret v)) -> Vectorized v

-- The first field is the String representation of the
-- value-level field.
data Shown :: Vec Height Bool -> Type where
  Shown ::
       String
    -> (Int -> Ground (Interpret v) -> ShowS)
    -> Shown v

-- A tree with value-showing functions.
showsPrecOmnitree :: Omnitree Height 'Zero Shown 'VecNil
showsPrecOmnitree = omnibuild singHeight
  (\v -> Shown
    (showIndexField v)
    (\p i -> showsPrecUniverse (interpret v) p i
    )
  )

-- TODO: Quit hard-coding the precedence. It has to get hard-coded
-- to something, but we should be sure that this number matches the
-- precedence of the actual infix operator.
showsRecord :: Show1 f
  => Tree Height 'Zero (Interpreted f) s 'VecNil
  -> ShowS
showsRecord t s0 = omnifoldr
  showsPrecOmnitree
  (\(Shown field sh) (Interpreted i) s -> showString "Value "
    . showString field
    . showChar ' '
    . liftShowsPrec sh (showList__ (sh 0)) 11 i
    $ (" &> " ++ s)
  )
  s0 t

type Singleton (v :: Vec Height Bool) =
  A.Singleton Height Height v 'SetFinLeaf 'GteEq

type Union (rs :: SetFin Height 'Zero) (ss :: SetFin Height 'Zero) =
  A.Union Height 'Zero rs ss

type family FromList (xs :: [Field]) :: SetFin Height 'Zero where
  FromList '[] = 'SetFinEmpty
  FromList (x ': xs) = Union (Singleton (Index x)) (FromList xs)

singleton ::
     SingField d
  -> f (Ground (Interpret (Index d)))
  -> Record f (Singleton (Index d))
singleton field value = Record (A.singleton A.SingGteEq finger finger (A.TreeLeaf (Interpreted value)))
  where
  finger = index field

leftUnion :: Record f rs -> Record f ss -> Record f (Union rs ss)
leftUnion (Record xs) (Record ys) = Record (A.leftUnion xs ys)

-- TODO: Should users be required to define this on their own?
(<.>) :: Record f rs -> Record f ss -> Record f (Union rs ss)
(<.>) = leftUnion

empty :: Record f 'SetFinEmpty
empty = Record A.empty

infixr 7 &>
(&>) :: Value f d -> Record f rs -> Record f (Union (Singleton (Index d)) rs)
Value d v &> rs = leftUnion (singleton d v) rs

type family Vectorize (w :: World Extra) :: Type where
  Vectorize ('W.Primitive p) = W.VectorizePrimitive p
  Vectorize ('W.Other e) = Array (InterpretExtra e)

vectorizedEquality :: forall (w :: World Extra) (s :: Type).
     SingWorld SingExtra w
  -> GroundWorld w
  -> Vectorize w
  -> MutablePrimArray s Word8
  -> Int -- offset
  -> Int -- length
  -> ST s ()
vectorizedEquality x = case x of
  W.SingPrimitive p -> case p of
    W.SingInt -> V.intEq
    W.SingChar -> V.charEq
    W.SingBool -> V.boolEq
  W.SingOther e -> V.liftedEq (eqExtra e)


vectorizedFilter ::
     SingWorld f w
  -> Vectorize w
  -> PrimArray Word8
  -> Int -- total number of matches
  -> Vectorize w
vectorizedFilter x = case x of
  W.SingPrimitive p -> case p of
    W.SingInt -> V.intFilter
    W.SingChar -> V.charFilter
    W.SingBool -> V.boolFilter
  W.SingOther _ -> V.liftedFilter

vectorizedIndex ::
     SingWorld f w
  -> Vectorize w
  -> Int
  -> GroundWorld w
vectorizedIndex !x !v !ix = case x of
  W.SingPrimitive p -> case p of
    W.SingInt -> PM.indexPrimArray v ix
    W.SingChar -> PM.indexPrimArray v ix
    W.SingBool -> PM.indexPrimArray v ix == 1
  W.SingOther _ -> PM.indexArray v ix

vectorizedAppend ::
     SingWorld f w
  -> Vectorize w
  -> Vectorize w
  -> Vectorize w
vectorizedAppend !x !v !w = case x of
  W.SingPrimitive p -> case p of
    W.SingInt -> v <> w
    W.SingChar -> v <> w
    W.SingBool -> v <> w
  W.SingOther _ -> v <> w

vectorizedSingleton ::
     SingWorld f w
  -> GroundWorld w
  -> Vectorize w
vectorizedSingleton !x !v = case x of
  W.SingPrimitive p -> case p of
    W.SingInt -> runST (PM.newPrimArray 1 >>= \a -> (PM.writePrimArray a 0 v *> PM.unsafeFreezePrimArray a))
    W.SingChar -> runST (PM.newPrimArray 1 >>= \a -> (PM.writePrimArray a 0 v *> PM.unsafeFreezePrimArray a))
    W.SingBool -> runST (PM.newPrimArray 1 >>= \a -> (PM.writePrimArray a 0 (fromIntegral @Int @Word8 (fromEnum v)) *> PM.unsafeFreezePrimArray a))
  W.SingOther _ -> runST (PM.newArray 1 v >>= \a -> PM.unsafeFreezeArray a)

one :: Record Identity rs -> Records rs
one (Record tree) = Records 1 $ A.map 
  (\fng (Interpreted (Identity v)) -> do
    let uni = interpret fng
     in Vectorized (vectorizedSingleton (represent uni) (toInternal uni v))
  ) FingerNil tree

toStructures :: Records rs -> Array (Record Identity rs)
toStructures (Records n tree) = runST $ do
  arr <- PM.newArray n errorThunk
  let go !ix = if ix >= 0
        then do
          let !r = A.map
                (\fng (Vectorized v) ->
                  let uni = interpret fng
                   in Interpreted (Identity (fromInternal uni (vectorizedIndex (represent uni) v ix)))
                )
                FingerNil tree
          PM.writeArray arr ix (Record r)
          go (ix - 1)
        else pure ()
  go (n - 1)
  PM.unsafeFreezeArray arr

-- summarize :: 
--      Record Proxy gs
--   -> Record (Elem)
--   -> Records rs
--   -> Records (Union gs as)
-- summarize 

tabulate :: Records rs -> IO ()
tabulate = mapM_ print . toStructures

errorThunk :: a
{-# noinline errorThunk #-}
errorThunk = error "Accordion.Record: errorThunk"
