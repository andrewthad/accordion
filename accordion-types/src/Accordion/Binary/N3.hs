{-# language DataKinds #-}
{-# language GADTs #-}
{-# language PatternSynonyms #-}
{-# language PolyKinds #-}
{-# language TypeApplications #-}

module Accordion.Binary.N3
  ( N0
  , N1
  , N2
  , N3
  , N4
  , N5
  , N6
  , N7
  , pattern N0
  , pattern N1
  , pattern N2
  , pattern N3
  , pattern N4
  , pattern N5
  , pattern N6
  , pattern N7
  ) where

import Accordion.Types (Vec(..))
import Accordion.Types (Finger(..),SingBool(..))
import qualified Accordion.Binary.N2 as N2
import qualified Accordion.Nat as Nat

type N0 = 'VecCons 'False N2.N0
type N1 = 'VecCons 'False N2.N1
type N2 = 'VecCons 'False N2.N2
type N3 = 'VecCons 'False N2.N3
type N4 = 'VecCons 'True N2.N0
type N5 = 'VecCons 'True N2.N1
type N6 = 'VecCons 'True N2.N2
type N7 = 'VecCons 'True N2.N3

pattern N0 :: () => (n ~ 'VecCons 'False N2.N0) => Finger @Nat.N3 n
pattern N0 = FingerCons SingFalse N2.N0

pattern N1 :: () => (n ~ 'VecCons 'False N2.N1) => Finger @Nat.N3 n
pattern N1 = FingerCons SingFalse N2.N1

pattern N2 :: () => (n ~ 'VecCons 'False N2.N2) => Finger @Nat.N3 n
pattern N2 = FingerCons SingFalse N2.N2

pattern N3 :: () => (n ~ 'VecCons 'False N2.N3) => Finger @Nat.N3 n
pattern N3 = FingerCons SingFalse N2.N3

pattern N4 :: () => (n ~ 'VecCons 'True N2.N0) => Finger @Nat.N3 n
pattern N4 = FingerCons SingTrue N2.N0

pattern N5 :: () => (n ~ 'VecCons 'True N2.N1) => Finger @Nat.N3 n
pattern N5 = FingerCons SingTrue N2.N1

pattern N6 :: () => (n ~ 'VecCons 'True N2.N2) => Finger @Nat.N3 n
pattern N6 = FingerCons SingTrue N2.N2

pattern N7 :: () => (n ~ 'VecCons 'True N2.N3) => Finger @Nat.N3 n
pattern N7 = FingerCons SingTrue N2.N3
