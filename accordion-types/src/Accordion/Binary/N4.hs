{-# language DataKinds #-}
{-# language GADTs #-}
{-# language PatternSynonyms #-}
{-# language PolyKinds #-}
{-# language TypeApplications #-}

module Accordion.Binary.N4
  ( N0
  , N1
  , N2
  , N3
  , N4
  , N5
  , N6
  , N7
  , N8
  , N9
  , N10
  , N11
  , N12
  , N13
  , N14
  , N15
  , pattern N0
  , pattern N1
  , pattern N2
  , pattern N3
  , pattern N4
  , pattern N5
  , pattern N6
  , pattern N7
  , pattern N8
  , pattern N9
  , pattern N10
  , pattern N11
  , pattern N12
  , pattern N13
  , pattern N14
  , pattern N15
  ) where

import Accordion.Types (Vec(..))
import Accordion.Types (Finger(..),SingBool(..))
import qualified Accordion.Binary.N3 as N3
import qualified Accordion.Nat as Nat

type N0 = 'VecCons 'False N3.N0
type N1 = 'VecCons 'False N3.N1
type N2 = 'VecCons 'False N3.N2
type N3 = 'VecCons 'False N3.N3
type N4 = 'VecCons 'False N3.N4
type N5 = 'VecCons 'False N3.N5
type N6 = 'VecCons 'False N3.N6
type N7 = 'VecCons 'False N3.N7
type N8 = 'VecCons 'True N3.N0
type N9 = 'VecCons 'True N3.N1
type N10 = 'VecCons 'True N3.N2
type N11 = 'VecCons 'True N3.N3
type N12 = 'VecCons 'True N3.N4
type N13 = 'VecCons 'True N3.N5
type N14 = 'VecCons 'True N3.N6
type N15 = 'VecCons 'True N3.N7

pattern N0 :: () => (n ~ 'VecCons 'False N3.N0) => Finger @Nat.N4 n
pattern N0 = FingerCons SingFalse N3.N0

pattern N1 :: () => (n ~ 'VecCons 'False N3.N1) => Finger @Nat.N4 n
pattern N1 = FingerCons SingFalse N3.N1

pattern N2 :: () => (n ~ 'VecCons 'False N3.N2) => Finger @Nat.N4 n
pattern N2 = FingerCons SingFalse N3.N2

pattern N3 :: () => (n ~ 'VecCons 'False N3.N3) => Finger @Nat.N4 n
pattern N3 = FingerCons SingFalse N3.N3

pattern N4 :: () => (n ~ 'VecCons 'False N3.N4) => Finger @Nat.N4 n
pattern N4 = FingerCons SingFalse N3.N4

pattern N5 :: () => (n ~ 'VecCons 'False N3.N5) => Finger @Nat.N4 n
pattern N5 = FingerCons SingFalse N3.N5

pattern N6 :: () => (n ~ 'VecCons 'False N3.N6) => Finger @Nat.N4 n
pattern N6 = FingerCons SingFalse N3.N6

pattern N7 :: () => (n ~ 'VecCons 'False N3.N7) => Finger @Nat.N4 n
pattern N7 = FingerCons SingFalse N3.N7

pattern N8 :: () => (n ~ 'VecCons 'True N3.N0) => Finger @Nat.N4 n
pattern N8 = FingerCons SingTrue N3.N0

pattern N9 :: () => (n ~ 'VecCons 'True N3.N1) => Finger @Nat.N4 n
pattern N9 = FingerCons SingTrue N3.N1

pattern N10 :: () => (n ~ 'VecCons 'True N3.N2) => Finger @Nat.N4 n
pattern N10 = FingerCons SingTrue N3.N2

pattern N11 :: () => (n ~ 'VecCons 'True N3.N3) => Finger @Nat.N4 n
pattern N11 = FingerCons SingTrue N3.N3

pattern N12 :: () => (n ~ 'VecCons 'True N3.N4) => Finger @Nat.N4 n
pattern N12 = FingerCons SingTrue N3.N4

pattern N13 :: () => (n ~ 'VecCons 'True N3.N5) => Finger @Nat.N4 n
pattern N13 = FingerCons SingTrue N3.N5

pattern N14 :: () => (n ~ 'VecCons 'True N3.N6) => Finger @Nat.N4 n
pattern N14 = FingerCons SingTrue N3.N6

pattern N15 :: () => (n ~ 'VecCons 'True N3.N7) => Finger @Nat.N4 n
pattern N15 = FingerCons SingTrue N3.N7
