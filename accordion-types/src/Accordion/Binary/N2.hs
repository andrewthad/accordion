{-# language DataKinds #-}
{-# language GADTs #-}
{-# language PatternSynonyms #-}
{-# language PolyKinds #-}
{-# language TypeApplications #-}

module Accordion.Binary.N2
  ( N0
  , N1
  , N2
  , N3
  , pattern N0
  , pattern N1
  , pattern N2
  , pattern N3
  ) where

import Accordion.Types (Vec(..))
import Accordion.Types (Finger(..),SingBool(..))
import qualified Accordion.Binary.N1 as N1
import qualified Accordion.Nat as Nat

type N0 = 'VecCons 'False N1.N0
type N1 = 'VecCons 'False N1.N1
type N2 = 'VecCons 'True N1.N0
type N3 = 'VecCons 'True N1.N1

pattern N0 :: () => (n ~ 'VecCons 'False N1.N0) => Finger @Nat.N2 n
pattern N0 = FingerCons SingFalse N1.N0

pattern N1 :: () => (n ~ 'VecCons 'False N1.N1) => Finger @Nat.N2 n
pattern N1 = FingerCons SingFalse N1.N1

pattern N2 :: () => (n ~ 'VecCons 'True N1.N0) => Finger @Nat.N2 n
pattern N2 = FingerCons SingTrue N1.N0

pattern N3 :: () => (n ~ 'VecCons 'True N1.N1) => Finger @Nat.N2 n
pattern N3 = FingerCons SingTrue N1.N1
