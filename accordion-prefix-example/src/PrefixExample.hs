{-# language DataKinds #-}
{-# language EmptyCase #-}
{-# language PolyKinds #-}
{-# language GADTs #-}
{-# language KindSignatures #-}
{-# language StandaloneDeriving #-}
{-# language TypeFamilies #-}
{-# language TypeOperators #-}
{-# language UndecidableInstances #-}

module PrefixExample where

import Data.Functor.Identity (Identity(Identity))
import Accordion.Types
import Data.Type.Equality ((:~:)(Refl))
import Data.Kind (Type)
import Accordion.World (World,SingWorld)
import Accordion.Nat (N2,N1)

import qualified Accordion.World as W

age' :: Int -> Tree
  (ApConst2 (Interpreted Identity))
  (Singleton (Index 'Age) ('MapLeaf '())) 'VecNil
age' i = singleton (index SingAge) (index SingAge)
  (TreeLeaf (ApConst2 (Interpreted (Identity i))))

alive' :: Bool -> Tree
  (ApConst2 (Interpreted Identity))
  (Singleton (Index 'Alive) ('MapLeaf '())) 'VecNil
alive' i = singleton (index SingAlive) (index SingAlive)
  (TreeLeaf (ApConst2 (Interpreted (Identity i))))

age :: Int -> Record (Interpreted Identity)
  ('Meta (Singleton (Index 'Age) ('MapLeaf '())) 'MapEmpty)
age i = Record (age' i) TreeEmpty

alive :: Bool -> Record (Interpreted Identity)
  ('Meta (Singleton (Index 'Alive) ('MapLeaf '())) 'MapEmpty)
alive i = Record (alive' i) TreeEmpty

dog :: Record f m
  -> Record f ('Meta 'MapEmpty (Singleton (IndexPrefix 'Dog) ('MapLeaf m)))
dog r = Record TreeEmpty
  (singleton
    (indexPrefix SingDog)
    (indexPrefix SingDog)
    (TreeLeaf (ApConst1 r))
  )

data Field
  = Age
  | Health
  | Letter
  | Alive

data Prefix
  = Dog
  | Cat

data SingField :: Field -> Type where
  SingAge :: SingField 'Age
  SingHealth :: SingField 'Health
  SingLetter :: SingField 'Letter
  SingAlive :: SingField 'Alive

deriving instance Show (SingField d)

data SingPrefix :: Prefix -> Type where
  SingDog :: SingPrefix 'Dog
  SingCat :: SingPrefix 'Cat

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

type family IndexPrefix (p :: Prefix) :: Vec N1 Bool where
  IndexPrefix 'Dog = 'VecCons 'True 'VecNil
  IndexPrefix 'Cat = 'VecCons 'False 'VecNil

newtype Interpreted :: (Type -> Type) -> Vec N2 Bool -> Type where
  Interpreted :: f (Ground (Interpret v)) -> Interpreted f v

type family Interpret (v :: Vec N2 Bool) :: Universe where
  Interpret x = InterpretField (Unindex x)

type family InterpretField (d :: Field) :: Universe where
  InterpretField 'Age = 'Number
  InterpretField 'Health = 'Number
  InterpretField 'Alive = 'Boolean
  InterpretField 'Letter = 'Character

type family Represent (u :: Universe) :: World Extra where
  Represent 'Number = ('W.Primitive 'W.Int)
  Represent 'Boolean = ('W.Primitive 'W.Bool)
  Represent 'Character = ('W.Primitive 'W.Char)

type family Ground (u :: Universe) :: Type where
  Ground 'Number = Int
  Ground 'Boolean = Bool
  Ground 'Character = Char

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

indexPrefix :: SingPrefix p -> Finger N1 (IndexPrefix p)
indexPrefix SingDog = FingerCons SingTrue FingerNil
indexPrefix SingCat = FingerCons SingFalse FingerNil

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

