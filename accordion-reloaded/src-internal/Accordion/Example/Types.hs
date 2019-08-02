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
  , Prefix(..)
  , SingField(..)
  , SingPrefix(..)
  , Extra
  , SingExtra
  , Represent
  , InterpretExtra
  , Universe(..)
  , SingUniverse(..)
    -- values
  , singFieldHeight
  , unindexField
  , unindexPrefix
  , index
  , interpret
  , indexRoundTrip
  , represent
  , showIndexField
  , eqExtra
  ) where

import Accordion.Types (Nat(..),Vec(..),Omnitree(..),SingNat,SingBool(..))
import Accordion.Types (Finger(..))
import Accordion.Nat (N2,n2,N1)
import Data.Kind (Type)
import Data.Type.Equality ((:~:)(Refl))
import Accordion.World (World,SingWorld)

import qualified Accordion.World as W

data Field
  = Age
  | Health
  | Letter
  | Alive

data Prefix
  = Source
  | Destination

data SingField :: Field -> Type where
  SingAge :: SingField 'Age
  SingHealth :: SingField 'Health
  SingLetter :: SingField 'Letter
  SingAlive :: SingField 'Alive

data SingPrefix :: Prefix -> Type where
  SingSource :: SingPrefix 'Source
  SingDestination :: SingPrefix 'Destination

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

type family UnindexPrefix (v :: Vec N1 Bool) :: Prefix where
  UnindexPrefix ('VecCons 'True 'VecNil) = 'Source
  UnindexPrefix ('VecCons 'False 'VecNil) = 'Destination

type family Index (d :: Field) :: Vec N2 Bool where
  Index 'Age = 'VecCons 'True ('VecCons 'True 'VecNil)
  Index 'Health = 'VecCons 'False ('VecCons 'True 'VecNil)
  Index 'Letter = 'VecCons 'True ('VecCons 'False 'VecNil)
  Index 'Alive = 'VecCons 'False ('VecCons 'False 'VecNil)

type family IndexPrefix (d :: Prefix) :: Vec N1 Bool where
  IndexPrefix 'Source = 'VecCons 'True 'VecNil
  IndexPrefix 'Destination = 'VecCons 'False 'VecNil

type family Interpret (v :: Vec N2 Bool) :: Universe where
  Interpret x = InterpretField (Unindex x)

type family InterpretField (d :: Field) :: Universe where
  InterpretField 'Age = 'Number
  InterpretField 'Health = 'Number
  InterpretField 'Alive = 'Boolean
  InterpretField 'Letter = 'Character

type family Represent (u :: Universe) :: World where
  Represent 'Number = ('W.World 'W.One 'W.Int)
  Represent 'Boolean = ('W.World 'W.One 'W.Bool)
  Represent 'Character = ('W.World 'W.One 'W.Char)

type family Ground (u :: Universe) :: Type where
  Ground 'Number = Int
  Ground 'Boolean = Bool
  Ground 'Character = Char

singFieldHeight :: SingNat N2
singFieldHeight = n2

showIndexField :: Finger N2 v -> String
showIndexField = show . unindexField

unindexField :: Finger N2 v -> SingField (Unindex v)
unindexField (FingerCons SingTrue (FingerCons SingTrue FingerNil)) = SingAge
unindexField (FingerCons SingFalse (FingerCons SingTrue FingerNil)) = SingHealth
unindexField (FingerCons SingTrue (FingerCons SingFalse FingerNil)) = SingLetter
unindexField (FingerCons SingFalse (FingerCons SingFalse FingerNil)) = SingAlive

unindexPrefix :: Finger N1 v -> SingPrefix (UnindexPrefix v)
unindexPrefix (FingerCons SingTrue FingerNil) = SingSource
unindexPrefix (FingerCons SingFalse FingerNil) = SingDestination

index :: SingField d -> Finger N2 (Index d)
index SingAge = (FingerCons SingTrue (FingerCons SingTrue FingerNil))
index SingHealth = (FingerCons SingFalse (FingerCons SingTrue FingerNil))
index SingLetter = (FingerCons SingTrue (FingerCons SingFalse FingerNil))
index SingAlive = (FingerCons SingFalse (FingerCons SingFalse FingerNil))

interpret :: Finger N2 v -> SingUniverse (Interpret v)
interpret f = case unindexField f of
  SingAge -> SingNumber
  SingHealth -> SingNumber
  SingAlive -> SingBoolean
  SingLetter -> SingCharacter

represent :: SingUniverse u -> SingWorld (Represent u)
represent SingNumber = W.SingWorld W.SingOne W.SingInt
represent SingCharacter = W.SingWorld W.SingOne W.SingChar
represent SingBoolean = W.SingWorld W.SingOne W.SingBool

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

