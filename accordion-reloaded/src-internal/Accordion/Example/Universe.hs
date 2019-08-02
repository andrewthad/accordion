{-# language DataKinds #-}
{-# language GADTSyntax #-}
{-# language KindSignatures #-}
{-# language TypeFamilies #-}
{-# language OverloadedStrings #-}

module Accordion.Example.Universe
  ( FieldHeight
  , PrefixHeight
  , ManyHeight
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
  , singFieldHeight
  , index
  , interpret
  , indexRoundTrip
  , represent
  , showIndexField
  , eqExtra
  , toInternal
  , fromInternal
  , encodeField
  , encodePrefix
  , pasteMany
  ) where

import Accordion.Example.Types

import Accordion.World (World(..),Primitive(..),GroundPrimitive(..),GroundWorld)
import Accordion.Types (Nat(..),Vec(..),Omnitree(..),Finger)
import Accordion.Nat (N2,N1)
import Data.Kind (Type)
import Data.Functor.Identity (Identity(..))
import Data.Text.Short (ShortText)

import qualified Accordion.Json.Types as Json
import qualified Accordion.Json.Encode as Encode
import qualified Accordion.Types as A
import qualified Accordion.World as A

type FieldHeight = N2
type PrefixHeight = N1
type ManyHeight = N1

showsPrecUniverse :: SingUniverse u -> Int -> Ground u -> ShowS
showsPrecUniverse SingNumber = showsPrec
showsPrecUniverse SingCharacter = showsPrec
showsPrecUniverse SingBoolean = showsPrec

toInternal ::
     SingUniverse u
  -> Ground u
  -> GroundWorld (Represent u)
toInternal n x = case n of
  SingCharacter -> Identity x
  SingNumber -> Identity x
  SingBoolean -> Identity x

fromInternal ::
     SingUniverse u
  -> GroundWorld (Represent u)
  -> Ground u
fromInternal n x = case n of
  SingCharacter -> runIdentity x
  SingNumber -> runIdentity x
  SingBoolean -> runIdentity x

encodeField :: Finger FieldHeight v -> ShortText
encodeField x = case unindexField x of
  SingAge -> "age"
  SingAlive -> "alive"
  SingHealth -> "health"
  SingLetter -> "letter"

encodePrefix :: Finger PrefixHeight v -> ShortText
encodePrefix x = case unindexPrefix x of
  SingSource -> "source"
  SingDestination -> "destination"

pasteMany ::
     Finger FieldHeight v
  -> Json.Encode (A.VectorizeWorld (Represent (Interpret v)))
pasteMany x = case unindexField x of
  SingAge -> Encode.int
  SingHealth -> Encode.int


