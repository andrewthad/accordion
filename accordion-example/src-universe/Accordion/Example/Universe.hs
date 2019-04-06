{-# language DataKinds #-}
{-# language GADTSyntax #-}
{-# language KindSignatures #-}
{-# language TypeFamilies #-}

module Accordion.Example.Universe
  ( Height
  , Field
  , Universe
  , Index
  , SingUniverse
  , SingField
  , Ground
  , Unindex
  , Interpret
  , showsPrecUniverse
  , singHeight
  , unindex
  , index
  , interpret
  , indexRoundTrip
  ) where

import Accordion.Example.Types

import Accordion.Types (Nat(..),Vec(..),Omnitree(..))
import Accordion.Nat (N2)
import Data.Kind (Type)

type Height = N2

showsPrecUniverse :: SingUniverse u -> Int -> Ground u -> ShowS
showsPrecUniverse SingNumber = showsPrec
showsPrecUniverse SingCharacter = showsPrec
showsPrecUniverse SingBoolean = showsPrec

