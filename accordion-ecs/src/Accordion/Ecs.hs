{-# language KindSignatures #-}
{-# language PolyKinds #-}
{-# language DataKinds #-}
{-# language GADTs #-}

module Accordion.Ecs
  ( Attributes(..)
  , Optionals(..)
  , column
  , nestedColumn
  , column1
  , single
  , single1
  , single2
  , nestedSingle
  , union
  , empty
  , toJson
    -- * Fields
  , port
  , question
  , classNumber
  , typeNumber
  , ip
    -- * Prefixes
  , dns
  , source
  ) where

import Accordion.Ecs.Optionals
import Accordion.Ecs.Types
import Accordion.Ecs.Json
import Data.Type.Equality ((:~:)(Refl))
import Accordion.World (VectorizeWorld)

import qualified Data.Primitive as PM
import qualified Arithmetic.Nat as Nat
import qualified Accordion.Ecs.Types as S
import qualified Arithmetic.Types as Arithmetic
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
    case Nat.testEqual n2 n1 of
      Nothing -> False
      Just equal -> if Word64.equals n1 t1 (Word64.substitute equal t2)
        then equalsHetero n1 a1 (substitute equal a2)
        else False

column1 ::
     A.Finger p1
  -> A.Finger f1
  -> Bool.Vector n
  -> VectorizeWorld (Represent (Interpret f1)) n
  -> Optionals n (A.NestedSingleton '[p1] f1 '())
column1 p1 f1 = nestedColumn (A.RecCons p1 A.RecNil) f1

single1 ::
     A.Finger p1
  -> A.Finger f1
  -> Ground (Interpret f1)
  -> Optionals 1 (A.NestedSingleton '[p1] f1 '())
single1 p1 f1 = nestedSingle (A.RecCons p1 A.RecNil) f1

single2 ::
     A.Finger p1
  -> A.Finger p2
  -> A.Finger f1
  -> Ground (Interpret f1)
  -> Optionals 1 (A.NestedSingleton '[p1,p2] f1 '())
single2 p1 p2 f1 = nestedSingle (A.RecCons p1 (A.RecCons p2 A.RecNil)) f1

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

ip :: A.Finger (Index 'Ip)
ip = index SingIp

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
