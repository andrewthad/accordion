{-# language DataKinds #-}

module Accordion.Example
  ( module Accordion.Example.Record
  , module Accordion.Example.Types
    -- Helpers
  , age
  , health
  , letter
  , alive
    -- Examples to play with in GHCi
  , fooA
  , fooB
  , fooC
  , fooD
  , foos
  ) where

import Accordion.Example.Record
import Accordion.Example.Types

import Data.Functor.Identity (Identity(Identity))

age :: Int -> Record Identity (Singleton (Index 'Age))
age = singleton SingAge . Identity

health :: Int -> Record Identity (Singleton (Index 'Health))
health = singleton SingHealth . Identity

letter :: Char -> Record Identity (Singleton (Index 'Letter))
letter = singleton SingLetter . Identity

alive :: Bool -> Record Identity (Singleton (Index 'Alive))
alive = singleton SingAlive . Identity

fooA,fooB,fooC,fooD :: Record Identity (FromList '[ 'Age, 'Letter])
fooA = letter 'y' <.> age 42
fooB = letter 'x' <.> age 34
fooC = letter 'z' <.> age 42
fooD = letter 'x' <.> age 13

foos :: Records (FromList '[ 'Age, 'Letter])
foos =
     one fooA
  <> one fooB
  <> one fooC
  <> one fooD
