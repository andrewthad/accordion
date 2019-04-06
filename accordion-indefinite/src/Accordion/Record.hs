{-# language BangPatterns #-}
{-# language DataKinds #-}
{-# language GADTs #-}
{-# language KindSignatures #-}
{-# language PolyKinds #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language TypeFamilies #-}
{-# language TypeInType #-}

module Accordion.Record
  ( -- Data types
    Record(..)
  , Value(..)
    -- Functions
  , empty
  , singleton
  , leftUnion
  , filter
  , (&>)
    -- Type families
  , Singleton
  , Union
  ) where

import Prelude hiding (filter)

import Accordion.Types (Nat(..),Vec(..),SetFin(..),Finger(..))
import Accordion.Types (Tree(..),Gte(GteEq),Omnitree(..))
import Accordion.Types (omnibuild,omnifoldr)
import Accordion.Universe (Height,Interpret,Unindex,Ground,Interpret)
import Accordion.Universe (SingField,Index,Field,Vectorize)
import Accordion.Universe (singHeight,showsPrecUniverse,unindex,interpret,index)
import Accordion.Universe (indexRoundTrip,vectorizedEquality,vectorizedFilter)
import Control.Monad.ST (runST)
import Data.Functor.Classes (Show1,liftShowsPrec)
import Data.Functor.Identity (Identity(..))
import Data.Primitive (MutablePrimArray,PrimArray)
import Data.Kind (Type)
import Data.Type.Equality ((:~:)(Refl))
import Data.Word (Word8)
import GHC.Show (showList__)

import qualified Accordion.Types as A
import qualified Data.Primitive as PM

newtype Record :: (Type -> Type) -> SetFin Height 'Zero -> Type where
  Record :: Tree Height 'Zero (Interpreted f) s 'VecNil -> Record f s

data Records :: SetFin Height 'Zero -> Type where
  Records :: !Int -- Number of rows
          -> Tree Height 'Zero Vectorized s 'VecNil -- Columns
          -> Records s

filter :: Record Identity rs -> Records ss -> Records ss
filter (Record r) (Records n s) =
  let selected :: PrimArray Word8
      selected = runST $ do
        (enabled :: MutablePrimArray s Word8) <- PM.newPrimArray n
        -- Start off by marking all rows as being preserved.
        PM.setPrimArray enabled 0 n (1 :: Word8)
        A.zipM_
          ( \finger (Interpreted (Identity x)) (Vectorized vec) ->
              vectorizedEquality (interpret (unindex finger)) x vec enabled 0 n
          ) FingerNil r s
        PM.unsafeFreezePrimArray enabled
      total = PM.foldlPrimArray' (\acc w -> acc + fromIntegral w) (0 :: Int) selected
      t = A.map (\finger (Vectorized vec) -> Vectorized (vectorizedFilter (interpret (unindex finger)) vec selected 0 total)) FingerNil s
   in Records total t
  

data Value :: (Type -> Type) -> Field -> Type where
  Value :: SingField d -> f (Ground (Interpret d)) -> Value f d

instance Show1 f => Show (Record f s) where
  -- Is the number in the showParen predicate supposed to
  -- be 7 or 8? Not sure.
  showsPrec p (Record t) = showParen (p > 7) (showsRecord t . showString "empty")

newtype Interpreted :: (Type -> Type) -> Vec Height Bool -> Type where
  Interpreted :: f (Ground (Interpret (Unindex v))) -> Interpreted f v

newtype Vectorized :: Vec Height Bool -> Type where
  Vectorized :: Vectorize (Interpret (Unindex v)) -> Vectorized v

-- The first field is the String representation of the
-- value-level field.
data Shown :: Vec Height Bool -> Type where
  Shown ::
       String
    -> (Int -> Ground (Interpret (Unindex v)) -> ShowS)
    -> Shown v

-- A tree with value-showing functions.
showsPrecOmnitree :: Omnitree Height 'Zero Shown 'VecNil
showsPrecOmnitree = omnibuild singHeight
  (\v -> Shown
    (show (unindex v))
    (\p i -> showsPrecUniverse (interpret (unindex v)) p i
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

singleton ::
     SingField d
  -> f (Ground (Interpret d))
  -> Record f (Singleton (Index d))
singleton field value = case indexRoundTrip field of
  Refl -> Record (A.singleton A.SingGteEq finger finger (A.TreeLeaf (Interpreted value)))
  where
  finger = index field

leftUnion :: Record f rs -> Record f ss -> Record f (Union rs ss)
leftUnion (Record xs) (Record ys) = Record (A.leftUnion xs ys)

empty :: Record f 'SetFinEmpty
empty = Record A.empty

infixr 7 &>
(&>) :: Value f d -> Record f rs -> Record f (Union (Singleton (Index d)) rs)
Value d v &> rs = leftUnion (singleton d v) rs
