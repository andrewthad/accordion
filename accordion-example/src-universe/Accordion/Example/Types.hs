{-# language DataKinds #-}
{-# language EmptyCase #-}
{-# language GADTs #-}
{-# language KindSignatures #-}
{-# language StandaloneDeriving #-}
{-# language TypeFamilies #-}
{-# language TypeOperators #-}
{-# language UndecidableInstances #-}
module Accordion.Example.Types
  ( Ground
  , Unindex
  , Interpret
  , Index
  , Field(..)
  , SingField(..)
  , Extra
  , SingExtra
  , Represent
  , InterpretExtra
  , Universe(..)
  , SingUniverse(..)
    -- values
  , singHeight
  , unindex
  , index
  , interpret
  , indexRoundTrip
  , represent
  , showIndexField
  , eqExtra
  ) where

import Accordion.Types (Nat(..),Vec(..),Omnitree(..),SingNat,SingBool(..))
import Accordion.Types (Finger(..))
import Accordion.Nat (N2,n2)
import Data.Kind (Type)
import Data.Type.Equality ((:~:)(Refl))
import Accordion.World (World,SingWorld)

import qualified Accordion.World as W

data Field
  = Age
  | Health
  | Letter
  | Alive

data SingField :: Field -> Type where
  SingAge :: SingField 'Age
  SingHealth :: SingField 'Health
  SingLetter :: SingField 'Letter
  SingAlive :: SingField 'Alive

deriving instance Show (SingField d)

data Universe
  = Number
  | Character
  | Boolean

data SingUniverse :: Universe -> Type where
  SingNumber :: SingUniverse 'Number
  SingCharacter :: SingUniverse 'Character
  SingBoolean :: SingUniverse 'Boolean

type family Unindex (v :: Vec N2 Bool) :: Field where
  Unindex ('VecCons 'True ('VecCons 'True 'VecNil)) = 'Age
  Unindex ('VecCons 'False ('VecCons 'True 'VecNil)) = 'Health
  Unindex ('VecCons 'True ('VecCons 'False 'VecNil)) = 'Letter
  Unindex ('VecCons 'False ('VecCons 'False 'VecNil)) = 'Alive

type family Index (d :: Field) :: Vec N2 Bool where
  Index 'Age = 'VecCons 'True ('VecCons 'True 'VecNil)
  Index 'Health = 'VecCons 'False ('VecCons 'True 'VecNil)
  Index 'Letter = 'VecCons 'True ('VecCons 'False 'VecNil)
  Index 'Alive = 'VecCons 'False ('VecCons 'False 'VecNil)

type family Interpret (v :: Vec N2 Bool) :: Universe where
  Interpret x = InterpretField (Unindex x)

type family InterpretField (d :: Field) :: Universe where
  Interpret 'Age = 'Number
  Interpret 'Health = 'Number
  Interpret 'Alive = 'Boolean
  Interpret 'Letter = 'Character

type family Represent (u :: Universe) :: World Extra where
  Represent 'Number = ('W.Primitive 'W.Int)
  Represent 'Boolean = ('W.Primitive 'W.Bool)
  Represent 'Character = ('W.Primitive 'W.Char)

type family Ground (u :: Universe) :: Type where
  Ground 'Number = Int
  Ground 'Boolean = Bool
  Ground 'Character = Char

singHeight :: SingNat N2
singHeight = n2

showIndexField :: Finger N2 v -> String
showIndexField = show . unindex

unindex :: Finger N2 v -> SingField (Unindex v)
unindex (FingerCons SingTrue (FingerCons SingTrue FingerNil)) = SingAge
unindex (FingerCons SingFalse (FingerCons SingTrue FingerNil)) = SingHealth
unindex (FingerCons SingTrue (FingerCons SingFalse FingerNil)) = SingLetter
unindex (FingerCons SingFalse (FingerCons SingFalse FingerNil)) = SingAlive

index :: SingField d -> Finger N2 (Index d)
index SingAge = (FingerCons SingTrue (FingerCons SingTrue FingerNil))
index SingHealth = (FingerCons SingFalse (FingerCons SingTrue FingerNil))
index SingLetter = (FingerCons SingTrue (FingerCons SingFalse FingerNil))
index SingAlive = (FingerCons SingFalse (FingerCons SingFalse FingerNil))

interpret :: Finger N2 v -> SingUniverse (Interpret v)
interpret f = case unindex f of
  SingAge -> SingNumber
  SingHealth -> SingNumber
  SingAlive -> SingBoolean
  SingLetter -> SingCharacter

represent :: SingUniverse u -> SingWorld SingExtra (Represent u)
represent SingNumber = W.SingPrimitive W.SingInt
represent SingCharacter = W.SingPrimitive W.SingChar
represent SingBoolean = W.SingPrimitive W.SingBool

indexRoundTrip :: SingField d -> (Unindex (Index d) :~: d)
indexRoundTrip SingAge = Refl
indexRoundTrip SingHealth = Refl
indexRoundTrip SingAlive = Refl
indexRoundTrip SingLetter = Refl

data Extra

data SingExtra :: Extra -> Type where

type family InterpretExtra (e :: Extra) :: Type where

eqExtra :: SingExtra e -> InterpretExtra e -> InterpretExtra e -> Bool
eqExtra s = case s of {}

