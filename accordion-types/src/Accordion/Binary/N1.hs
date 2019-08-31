{-# language DataKinds #-}
{-# language GADTs #-}
{-# language PatternSynonyms #-}
{-# language PolyKinds #-}
{-# language TypeApplications #-}

module Accordion.Binary.N1
  ( N0
  , N1
  , n0
  , n1
  , pattern N0
  , pattern N1
  ) where

import Accordion.Types (Vec(..))
import Accordion.Types (Finger(..),SingBool(..))
import qualified Accordion.Binary.N0 as N0
import qualified Accordion.Nat as Nat

type N0 = 'VecCons 'False N0.N0
type N1 = 'VecCons 'True N0.N0

pattern N0 :: () => (n ~ 'VecCons 'False 'VecNil) => Finger @Nat.N1 n
pattern N0 = FingerCons SingFalse FingerNil

pattern N1 :: () => (n ~ 'VecCons 'True 'VecNil) => Finger @Nat.N1 n
pattern N1 = FingerCons SingTrue FingerNil

n0 :: Finger N0
n0 = FingerCons SingFalse FingerNil

n1 :: Finger N1
n1 = FingerCons SingTrue FingerNil

