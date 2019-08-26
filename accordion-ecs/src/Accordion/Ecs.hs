{-# language KindSignatures #-}
{-# language DataKinds #-}
{-# language GADTs #-}

module Accordion.Ecs
  ( Attributes(..)
  , Optionals(..)
  , column
  , nestedColumn
  , empty
    -- * Fields
  , port
  , dns
  , question
  , classNumber
  ) where

import Accordion.Ecs.Optionals
import Accordion.Ecs.Types
import Data.Array.Word64 (Word64Vector)

import qualified Accordion.Ecs.Types as S
import qualified Data.Arithmetic.Types as Arithmetic
import qualified GHC.TypeNats as GHC
import qualified Accordion.Types as A

data Attributes = forall (n :: GHC.Nat) (m :: A.Meta S.FieldHeight S.PrefixHeight S.ManyHeight). Attributes
  !(Arithmetic.Nat n)
  !(Word64Vector n) -- timestamps
  !(Optionals n m) -- optional attributes

port :: A.Finger (Index 'Port)
port = index SingPort

classNumber :: A.Finger (Index 'ClassNumber)
classNumber = index SingClassNumber

dns :: A.Finger (IndexPrefix 'Dns)
dns = indexPrefix SingDns

question :: A.Finger (IndexPrefix 'Question)
question = indexPrefix SingQuestion
