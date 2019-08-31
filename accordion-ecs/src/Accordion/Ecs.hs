{-# language KindSignatures #-}
{-# language DataKinds #-}
{-# language GADTs #-}

module Accordion.Ecs
  ( Attributes(..)
  , Optionals(..)
  , column
  , nestedColumn
  , single
  , nestedSingle
  , union
  , empty
  , toJson
    -- * Fields
  , port
  , question
  , classNumber
  , typeNumber
    -- * Prefixes
  , dns
  , source
  ) where

import Accordion.Ecs.Optionals
import Accordion.Ecs.Types
import Accordion.Ecs.Json
import Data.Type.Equality ((:~:)(Refl))

import qualified Data.Primitive as PM
import qualified Data.Arithmetic.Nat as Nat
import qualified Accordion.Ecs.Types as S
import qualified Data.Arithmetic.Types as Arithmetic
import qualified World.Bool as Bool
import qualified GHC.TypeNats as GHC
import qualified Accordion.Types as A
import qualified World.Word64 as Word64

data Attributes = forall (n :: GHC.Nat) (m :: A.Meta S.FieldHeight S.PrefixHeight S.ManyHeight). Attributes
  !(Arithmetic.Nat n)
  !(Word64.Vector n) -- timestamps
  !(Optionals n m) -- optional attributes

instance Eq Attributes where
  Attributes n1 t1 a1 == Attributes n2 t2 a2 =
    case Nat.equals n2 n1 of
      Nothing -> False
      Just equal -> if Word64.equals n1 t1 (Word64.substitute equal t2)
        then equalsHetero n1 a1 (substitute equal a2)
        else False

toJson :: Attributes -> PM.Array PM.ByteArray
toJson (Attributes n ts rs) = encodeOptionals 5 n
  ( union
    (column timestamp (Bool.replicate n True) ts)
    rs
  )

timestamp :: A.Finger (Index 'Timestamp)
timestamp = index SingTimestamp

port :: A.Finger (Index 'Port)
port = index SingPort

classNumber :: A.Finger (Index 'ClassNumber)
classNumber = index SingClassNumber

typeNumber :: A.Finger (Index 'TypeNumber)
typeNumber = index SingTypeNumber

dns :: A.Finger (IndexPrefix 'Dns)
dns = indexPrefix SingDns

source :: A.Finger (IndexPrefix 'Source)
source = indexPrefix SingSource

question :: A.Finger (IndexPrefix 'Question)
question = indexPrefix SingQuestion
