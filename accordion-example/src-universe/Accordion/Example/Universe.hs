{-# language DataKinds #-}
{-# language GADTSyntax #-}
{-# language KindSignatures #-}
{-# language TypeFamilies #-}

module Accordion.Example.Universe
  ( Height
  , Field
  , SingExtra
  , Extra
  , Universe
  , Index
  , SingUniverse
  , SingField
  , Ground
  , GroundWorld
  , Unindex
  , Interpret
  , Represent
  , InterpretExtra
  , showsPrecUniverse
  , singHeight
  , unindex
  , index
  , interpret
  , indexRoundTrip
  , represent
  , showIndexField
  , eqExtra
  , toInternal
  , fromInternal
  ) where

import Accordion.Example.Types

import Accordion.World (World(..),Primitive(..),GroundPrimitive(..))
import Accordion.Types (Nat(..),Vec(..),Omnitree(..))
import Accordion.Nat (N2)
import Data.Kind (Type)

type Height = N2

showsPrecUniverse :: SingUniverse u -> Int -> Ground u -> ShowS
showsPrecUniverse SingNumber = showsPrec
showsPrecUniverse SingCharacter = showsPrec
showsPrecUniverse SingBoolean = showsPrec

type family GroundWorld (w :: World Extra) :: Type where
  GroundWorld ('Primitive p) = GroundPrimitive p
  GroundWorld ('Other e) = InterpretExtra e

toInternal ::
     SingUniverse u
  -> Ground u
  -> GroundWorld (Represent u)
toInternal n x = case n of
  SingCharacter -> x
  SingNumber -> x
  SingBoolean -> x

fromInternal ::
     SingUniverse u
  -> GroundWorld (Represent u)
  -> Ground u
fromInternal n x = case n of
  SingCharacter -> x
  SingNumber -> x
  SingBoolean -> x
