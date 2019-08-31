{-# language DataKinds #-}
{-# language GADTs #-}
{-# language PatternSynonyms #-}
{-# language PolyKinds #-}
{-# language TypeApplications #-}

module Accordion.Binary.N5
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
  , N16
  , N17
  , N18
  , N19
  , N20
  , N21
  , N22
  , N23
  , N24
  , N25
  , N26
  , N27
  , N28
  , N29
  , N30
  , N31
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
  , pattern N16
  , pattern N17
  , pattern N18
  , pattern N19
  , pattern N20
  , pattern N21
  , pattern N22
  , pattern N23
  , pattern N24
  , pattern N25
  , pattern N26
  , pattern N27
  , pattern N28
  , pattern N29
  , pattern N30
  , pattern N31
  ) where

import Accordion.Types (Vec(..))
import Accordion.Types (Finger(..),SingBool(..))
import qualified Accordion.Binary.N4 as N4
import qualified Accordion.Nat as Nat

type N0 = 'VecCons 'False N4.N0
type N1 = 'VecCons 'False N4.N1
type N2 = 'VecCons 'False N4.N2
type N3 = 'VecCons 'False N4.N3
type N4 = 'VecCons 'False N4.N4
type N5 = 'VecCons 'False N4.N5
type N6 = 'VecCons 'False N4.N6
type N7 = 'VecCons 'False N4.N7
type N8 = 'VecCons 'False N4.N8
type N9 = 'VecCons 'False N4.N9
type N10 = 'VecCons 'False N4.N10
type N11 = 'VecCons 'False N4.N11
type N12 = 'VecCons 'False N4.N12
type N13 = 'VecCons 'False N4.N13
type N14 = 'VecCons 'False N4.N14
type N15 = 'VecCons 'False N4.N15
type N16 = 'VecCons 'True N4.N0
type N17 = 'VecCons 'True N4.N1
type N18 = 'VecCons 'True N4.N2
type N19 = 'VecCons 'True N4.N3
type N20 = 'VecCons 'True N4.N4
type N21 = 'VecCons 'True N4.N5
type N22 = 'VecCons 'True N4.N6
type N23 = 'VecCons 'True N4.N7
type N24 = 'VecCons 'True N4.N8
type N25 = 'VecCons 'True N4.N9
type N26 = 'VecCons 'True N4.N10
type N27 = 'VecCons 'True N4.N11
type N28 = 'VecCons 'True N4.N12
type N29 = 'VecCons 'True N4.N13
type N30 = 'VecCons 'True N4.N14
type N31 = 'VecCons 'True N4.N15

pattern N0 :: () => (n ~ 'VecCons 'False N4.N0) => Finger @Nat.N5 n
pattern N0 = FingerCons SingFalse N4.N0

pattern N1 :: () => (n ~ 'VecCons 'False N4.N1) => Finger @Nat.N5 n
pattern N1 = FingerCons SingFalse N4.N1

pattern N2 :: () => (n ~ 'VecCons 'False N4.N2) => Finger @Nat.N5 n
pattern N2 = FingerCons SingFalse N4.N2

pattern N3 :: () => (n ~ 'VecCons 'False N4.N3) => Finger @Nat.N5 n
pattern N3 = FingerCons SingFalse N4.N3

pattern N4 :: () => (n ~ 'VecCons 'False N4.N4) => Finger @Nat.N5 n
pattern N4 = FingerCons SingFalse N4.N4

pattern N5 :: () => (n ~ 'VecCons 'False N4.N5) => Finger @Nat.N5 n
pattern N5 = FingerCons SingFalse N4.N5

pattern N6 :: () => (n ~ 'VecCons 'False N4.N6) => Finger @Nat.N5 n
pattern N6 = FingerCons SingFalse N4.N6

pattern N7 :: () => (n ~ 'VecCons 'False N4.N7) => Finger @Nat.N5 n
pattern N7 = FingerCons SingFalse N4.N7

pattern N8 :: () => (n ~ 'VecCons 'False N4.N8) => Finger @Nat.N5 n
pattern N8 = FingerCons SingFalse N4.N8

pattern N9 :: () => (n ~ 'VecCons 'False N4.N9) => Finger @Nat.N5 n
pattern N9 = FingerCons SingFalse N4.N9

pattern N10 :: () => (n ~ 'VecCons 'False N4.N10) => Finger @Nat.N5 n
pattern N10 = FingerCons SingFalse N4.N10

pattern N11 :: () => (n ~ 'VecCons 'False N4.N11) => Finger @Nat.N5 n
pattern N11 = FingerCons SingFalse N4.N11

pattern N12 :: () => (n ~ 'VecCons 'False N4.N12) => Finger @Nat.N5 n
pattern N12 = FingerCons SingFalse N4.N12

pattern N13 :: () => (n ~ 'VecCons 'False N4.N13) => Finger @Nat.N5 n
pattern N13 = FingerCons SingFalse N4.N13

pattern N14 :: () => (n ~ 'VecCons 'False N4.N14) => Finger @Nat.N5 n
pattern N14 = FingerCons SingFalse N4.N14

pattern N15 :: () => (n ~ 'VecCons 'False N4.N15) => Finger @Nat.N5 n
pattern N15 = FingerCons SingFalse N4.N15

pattern N16 :: () => (n ~ 'VecCons 'True N4.N0) => Finger @Nat.N5 n
pattern N16 = FingerCons SingTrue N4.N0

pattern N17 :: () => (n ~ 'VecCons 'True N4.N1) => Finger @Nat.N5 n
pattern N17 = FingerCons SingTrue N4.N1

pattern N18 :: () => (n ~ 'VecCons 'True N4.N2) => Finger @Nat.N5 n
pattern N18 = FingerCons SingTrue N4.N2

pattern N19 :: () => (n ~ 'VecCons 'True N4.N3) => Finger @Nat.N5 n
pattern N19 = FingerCons SingTrue N4.N3

pattern N20 :: () => (n ~ 'VecCons 'True N4.N4) => Finger @Nat.N5 n
pattern N20 = FingerCons SingTrue N4.N4

pattern N21 :: () => (n ~ 'VecCons 'True N4.N5) => Finger @Nat.N5 n
pattern N21 = FingerCons SingTrue N4.N5

pattern N22 :: () => (n ~ 'VecCons 'True N4.N6) => Finger @Nat.N5 n
pattern N22 = FingerCons SingTrue N4.N6

pattern N23 :: () => (n ~ 'VecCons 'True N4.N7) => Finger @Nat.N5 n
pattern N23 = FingerCons SingTrue N4.N7

pattern N24 :: () => (n ~ 'VecCons 'True N4.N8) => Finger @Nat.N5 n
pattern N24 = FingerCons SingTrue N4.N8

pattern N25 :: () => (n ~ 'VecCons 'True N4.N9) => Finger @Nat.N5 n
pattern N25 = FingerCons SingTrue N4.N9

pattern N26 :: () => (n ~ 'VecCons 'True N4.N10) => Finger @Nat.N5 n
pattern N26 = FingerCons SingTrue N4.N10

pattern N27 :: () => (n ~ 'VecCons 'True N4.N11) => Finger @Nat.N5 n
pattern N27 = FingerCons SingTrue N4.N11

pattern N28 :: () => (n ~ 'VecCons 'True N4.N12) => Finger @Nat.N5 n
pattern N28 = FingerCons SingTrue N4.N12

pattern N29 :: () => (n ~ 'VecCons 'True N4.N13) => Finger @Nat.N5 n
pattern N29 = FingerCons SingTrue N4.N13

pattern N30 :: () => (n ~ 'VecCons 'True N4.N14) => Finger @Nat.N5 n
pattern N30 = FingerCons SingTrue N4.N14

pattern N31 :: () => (n ~ 'VecCons 'True N4.N15) => Finger @Nat.N5 n
pattern N31 = FingerCons SingTrue N4.N15
