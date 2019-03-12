{-# language DataKinds #-}
{-# language GADTSyntax #-}
{-# language KindSignatures #-}
{-# language TypeFamilies #-}
module Accordion.Example.Types
  ( Interpret(..)
  , Ground
    -- type synonyms
  , Age
  , Health
  , Letter
  , Alive
  ) where

import Accordion.Types (Nat(..),Vec(..),Shown(..),Omnitree(..))
import Accordion.Nat (N2)
import Data.Kind (Type)

newtype Interpret :: Vec N2 Bool -> Type where
  Interpret :: { getInterpret :: Ground v } -> Interpret v

type family Ground (v :: Vec N2 Bool) :: Type where
  Ground ('VecCons 'True ('VecCons 'True 'VecNil)) = Int
  Ground ('VecCons 'True ('VecCons 'False 'VecNil)) = Word
  Ground ('VecCons 'False ('VecCons 'True 'VecNil)) = Char
  Ground ('VecCons 'False ('VecCons 'False 'VecNil)) = Bool

type Age = 'VecCons 'True ('VecCons 'True 'VecNil)
type Health = 'VecCons 'True ('VecCons 'False 'VecNil)
type Letter = 'VecCons 'False ('VecCons 'True 'VecNil)
type Alive = 'VecCons 'False ('VecCons 'False 'VecNil)


