{-# language DataKinds #-}
{-# language GADTs #-}
{-# language PatternSynonyms #-}
{-# language PolyKinds #-}
{-# language TypeApplications #-}

module Accordion.Binary.N6
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
  , N32
  , N33
  , N34
  , N35
  , N36
  , N37
  , N38
  , N39
  , N40
  , N41
  , N42
  , N43
  , N44
  , N45
  , N46
  , N47
  , N48
  , N49
  , N50
  , N51
  , N52
  , N53
  , N54
  , N55
  , N56
  , N57
  , N58
  , N59
  , N60
  , N61
  , N62
  , N63
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
  , pattern N32
  , pattern N33
  , pattern N34
  , pattern N35
  , pattern N36
  , pattern N37
  , pattern N38
  , pattern N39
  , pattern N40
  , pattern N41
  , pattern N42
  , pattern N43
  , pattern N44
  , pattern N45
  , pattern N46
  , pattern N47
  , pattern N48
  , pattern N49
  , pattern N50
  , pattern N51
  , pattern N52
  , pattern N53
  , pattern N54
  , pattern N55
  , pattern N56
  , pattern N57
  , pattern N58
  , pattern N59
  , pattern N60
  , pattern N61
  , pattern N62
  , pattern N63
  ) where

import Accordion.Types (Vec(..))
import Accordion.Types (Finger(..),SingBool(..))
import qualified Accordion.Binary.N5 as N5
import qualified Accordion.Nat as Nat

type N0 = 'VecCons 'False N5.N0
type N1 = 'VecCons 'False N5.N1
type N2 = 'VecCons 'False N5.N2
type N3 = 'VecCons 'False N5.N3
type N4 = 'VecCons 'False N5.N4
type N5 = 'VecCons 'False N5.N5
type N6 = 'VecCons 'False N5.N6
type N7 = 'VecCons 'False N5.N7
type N8 = 'VecCons 'False N5.N8
type N9 = 'VecCons 'False N5.N9
type N10 = 'VecCons 'False N5.N10
type N11 = 'VecCons 'False N5.N11
type N12 = 'VecCons 'False N5.N12
type N13 = 'VecCons 'False N5.N13
type N14 = 'VecCons 'False N5.N14
type N15 = 'VecCons 'False N5.N15
type N16 = 'VecCons 'False N5.N16
type N17 = 'VecCons 'False N5.N17
type N18 = 'VecCons 'False N5.N18
type N19 = 'VecCons 'False N5.N19
type N20 = 'VecCons 'False N5.N20
type N21 = 'VecCons 'False N5.N21
type N22 = 'VecCons 'False N5.N22
type N23 = 'VecCons 'False N5.N23
type N24 = 'VecCons 'False N5.N24
type N25 = 'VecCons 'False N5.N25
type N26 = 'VecCons 'False N5.N26
type N27 = 'VecCons 'False N5.N27
type N28 = 'VecCons 'False N5.N28
type N29 = 'VecCons 'False N5.N29
type N30 = 'VecCons 'False N5.N30
type N31 = 'VecCons 'False N5.N31
type N32 = 'VecCons 'True N5.N0
type N33 = 'VecCons 'True N5.N1
type N34 = 'VecCons 'True N5.N2
type N35 = 'VecCons 'True N5.N3
type N36 = 'VecCons 'True N5.N4
type N37 = 'VecCons 'True N5.N5
type N38 = 'VecCons 'True N5.N6
type N39 = 'VecCons 'True N5.N7
type N40 = 'VecCons 'True N5.N8
type N41 = 'VecCons 'True N5.N9
type N42 = 'VecCons 'True N5.N10
type N43 = 'VecCons 'True N5.N11
type N44 = 'VecCons 'True N5.N12
type N45 = 'VecCons 'True N5.N13
type N46 = 'VecCons 'True N5.N14
type N47 = 'VecCons 'True N5.N15
type N48 = 'VecCons 'True N5.N16
type N49 = 'VecCons 'True N5.N17
type N50 = 'VecCons 'True N5.N18
type N51 = 'VecCons 'True N5.N19
type N52 = 'VecCons 'True N5.N20
type N53 = 'VecCons 'True N5.N21
type N54 = 'VecCons 'True N5.N22
type N55 = 'VecCons 'True N5.N23
type N56 = 'VecCons 'True N5.N24
type N57 = 'VecCons 'True N5.N25
type N58 = 'VecCons 'True N5.N26
type N59 = 'VecCons 'True N5.N27
type N60 = 'VecCons 'True N5.N28
type N61 = 'VecCons 'True N5.N29
type N62 = 'VecCons 'True N5.N30
type N63 = 'VecCons 'True N5.N31

pattern N0 :: () => (n ~ 'VecCons 'False N5.N0) => Finger @Nat.N6 n
pattern N0 = FingerCons SingFalse N5.N0

pattern N1 :: () => (n ~ 'VecCons 'False N5.N1) => Finger @Nat.N6 n
pattern N1 = FingerCons SingFalse N5.N1

pattern N2 :: () => (n ~ 'VecCons 'False N5.N2) => Finger @Nat.N6 n
pattern N2 = FingerCons SingFalse N5.N2

pattern N3 :: () => (n ~ 'VecCons 'False N5.N3) => Finger @Nat.N6 n
pattern N3 = FingerCons SingFalse N5.N3

pattern N4 :: () => (n ~ 'VecCons 'False N5.N4) => Finger @Nat.N6 n
pattern N4 = FingerCons SingFalse N5.N4

pattern N5 :: () => (n ~ 'VecCons 'False N5.N5) => Finger @Nat.N6 n
pattern N5 = FingerCons SingFalse N5.N5

pattern N6 :: () => (n ~ 'VecCons 'False N5.N6) => Finger @Nat.N6 n
pattern N6 = FingerCons SingFalse N5.N6

pattern N7 :: () => (n ~ 'VecCons 'False N5.N7) => Finger @Nat.N6 n
pattern N7 = FingerCons SingFalse N5.N7

pattern N8 :: () => (n ~ 'VecCons 'False N5.N8) => Finger @Nat.N6 n
pattern N8 = FingerCons SingFalse N5.N8

pattern N9 :: () => (n ~ 'VecCons 'False N5.N9) => Finger @Nat.N6 n
pattern N9 = FingerCons SingFalse N5.N9

pattern N10 :: () => (n ~ 'VecCons 'False N5.N10) => Finger @Nat.N6 n
pattern N10 = FingerCons SingFalse N5.N10

pattern N11 :: () => (n ~ 'VecCons 'False N5.N11) => Finger @Nat.N6 n
pattern N11 = FingerCons SingFalse N5.N11

pattern N12 :: () => (n ~ 'VecCons 'False N5.N12) => Finger @Nat.N6 n
pattern N12 = FingerCons SingFalse N5.N12

pattern N13 :: () => (n ~ 'VecCons 'False N5.N13) => Finger @Nat.N6 n
pattern N13 = FingerCons SingFalse N5.N13

pattern N14 :: () => (n ~ 'VecCons 'False N5.N14) => Finger @Nat.N6 n
pattern N14 = FingerCons SingFalse N5.N14

pattern N15 :: () => (n ~ 'VecCons 'False N5.N15) => Finger @Nat.N6 n
pattern N15 = FingerCons SingFalse N5.N15

pattern N16 :: () => (n ~ 'VecCons 'False N5.N16) => Finger @Nat.N6 n
pattern N16 = FingerCons SingFalse N5.N16

pattern N17 :: () => (n ~ 'VecCons 'False N5.N17) => Finger @Nat.N6 n
pattern N17 = FingerCons SingFalse N5.N17

pattern N18 :: () => (n ~ 'VecCons 'False N5.N18) => Finger @Nat.N6 n
pattern N18 = FingerCons SingFalse N5.N18

pattern N19 :: () => (n ~ 'VecCons 'False N5.N19) => Finger @Nat.N6 n
pattern N19 = FingerCons SingFalse N5.N19

pattern N20 :: () => (n ~ 'VecCons 'False N5.N20) => Finger @Nat.N6 n
pattern N20 = FingerCons SingFalse N5.N20

pattern N21 :: () => (n ~ 'VecCons 'False N5.N21) => Finger @Nat.N6 n
pattern N21 = FingerCons SingFalse N5.N21

pattern N22 :: () => (n ~ 'VecCons 'False N5.N22) => Finger @Nat.N6 n
pattern N22 = FingerCons SingFalse N5.N22

pattern N23 :: () => (n ~ 'VecCons 'False N5.N23) => Finger @Nat.N6 n
pattern N23 = FingerCons SingFalse N5.N23

pattern N24 :: () => (n ~ 'VecCons 'False N5.N24) => Finger @Nat.N6 n
pattern N24 = FingerCons SingFalse N5.N24

pattern N25 :: () => (n ~ 'VecCons 'False N5.N25) => Finger @Nat.N6 n
pattern N25 = FingerCons SingFalse N5.N25

pattern N26 :: () => (n ~ 'VecCons 'False N5.N26) => Finger @Nat.N6 n
pattern N26 = FingerCons SingFalse N5.N26

pattern N27 :: () => (n ~ 'VecCons 'False N5.N27) => Finger @Nat.N6 n
pattern N27 = FingerCons SingFalse N5.N27

pattern N28 :: () => (n ~ 'VecCons 'False N5.N28) => Finger @Nat.N6 n
pattern N28 = FingerCons SingFalse N5.N28

pattern N29 :: () => (n ~ 'VecCons 'False N5.N29) => Finger @Nat.N6 n
pattern N29 = FingerCons SingFalse N5.N29

pattern N30 :: () => (n ~ 'VecCons 'False N5.N30) => Finger @Nat.N6 n
pattern N30 = FingerCons SingFalse N5.N30

pattern N31 :: () => (n ~ 'VecCons 'False N5.N31) => Finger @Nat.N6 n
pattern N31 = FingerCons SingFalse N5.N31

pattern N32 :: () => (n ~ 'VecCons 'True N5.N0) => Finger @Nat.N6 n
pattern N32 = FingerCons SingTrue N5.N0

pattern N33 :: () => (n ~ 'VecCons 'True N5.N1) => Finger @Nat.N6 n
pattern N33 = FingerCons SingTrue N5.N1

pattern N34 :: () => (n ~ 'VecCons 'True N5.N2) => Finger @Nat.N6 n
pattern N34 = FingerCons SingTrue N5.N2

pattern N35 :: () => (n ~ 'VecCons 'True N5.N3) => Finger @Nat.N6 n
pattern N35 = FingerCons SingTrue N5.N3

pattern N36 :: () => (n ~ 'VecCons 'True N5.N4) => Finger @Nat.N6 n
pattern N36 = FingerCons SingTrue N5.N4

pattern N37 :: () => (n ~ 'VecCons 'True N5.N5) => Finger @Nat.N6 n
pattern N37 = FingerCons SingTrue N5.N5

pattern N38 :: () => (n ~ 'VecCons 'True N5.N6) => Finger @Nat.N6 n
pattern N38 = FingerCons SingTrue N5.N6

pattern N39 :: () => (n ~ 'VecCons 'True N5.N7) => Finger @Nat.N6 n
pattern N39 = FingerCons SingTrue N5.N7

pattern N40 :: () => (n ~ 'VecCons 'True N5.N8) => Finger @Nat.N6 n
pattern N40 = FingerCons SingTrue N5.N8

pattern N41 :: () => (n ~ 'VecCons 'True N5.N9) => Finger @Nat.N6 n
pattern N41 = FingerCons SingTrue N5.N9

pattern N42 :: () => (n ~ 'VecCons 'True N5.N10) => Finger @Nat.N6 n
pattern N42 = FingerCons SingTrue N5.N10

pattern N43 :: () => (n ~ 'VecCons 'True N5.N11) => Finger @Nat.N6 n
pattern N43 = FingerCons SingTrue N5.N11

pattern N44 :: () => (n ~ 'VecCons 'True N5.N12) => Finger @Nat.N6 n
pattern N44 = FingerCons SingTrue N5.N12

pattern N45 :: () => (n ~ 'VecCons 'True N5.N13) => Finger @Nat.N6 n
pattern N45 = FingerCons SingTrue N5.N13

pattern N46 :: () => (n ~ 'VecCons 'True N5.N14) => Finger @Nat.N6 n
pattern N46 = FingerCons SingTrue N5.N14

pattern N47 :: () => (n ~ 'VecCons 'True N5.N15) => Finger @Nat.N6 n
pattern N47 = FingerCons SingTrue N5.N15

pattern N48 :: () => (n ~ 'VecCons 'True N5.N16) => Finger @Nat.N6 n
pattern N48 = FingerCons SingTrue N5.N16

pattern N49 :: () => (n ~ 'VecCons 'True N5.N17) => Finger @Nat.N6 n
pattern N49 = FingerCons SingTrue N5.N17

pattern N50 :: () => (n ~ 'VecCons 'True N5.N18) => Finger @Nat.N6 n
pattern N50 = FingerCons SingTrue N5.N18

pattern N51 :: () => (n ~ 'VecCons 'True N5.N19) => Finger @Nat.N6 n
pattern N51 = FingerCons SingTrue N5.N19

pattern N52 :: () => (n ~ 'VecCons 'True N5.N20) => Finger @Nat.N6 n
pattern N52 = FingerCons SingTrue N5.N20

pattern N53 :: () => (n ~ 'VecCons 'True N5.N21) => Finger @Nat.N6 n
pattern N53 = FingerCons SingTrue N5.N21

pattern N54 :: () => (n ~ 'VecCons 'True N5.N22) => Finger @Nat.N6 n
pattern N54 = FingerCons SingTrue N5.N22

pattern N55 :: () => (n ~ 'VecCons 'True N5.N23) => Finger @Nat.N6 n
pattern N55 = FingerCons SingTrue N5.N23

pattern N56 :: () => (n ~ 'VecCons 'True N5.N24) => Finger @Nat.N6 n
pattern N56 = FingerCons SingTrue N5.N24

pattern N57 :: () => (n ~ 'VecCons 'True N5.N25) => Finger @Nat.N6 n
pattern N57 = FingerCons SingTrue N5.N25

pattern N58 :: () => (n ~ 'VecCons 'True N5.N26) => Finger @Nat.N6 n
pattern N58 = FingerCons SingTrue N5.N26

pattern N59 :: () => (n ~ 'VecCons 'True N5.N27) => Finger @Nat.N6 n
pattern N59 = FingerCons SingTrue N5.N27

pattern N60 :: () => (n ~ 'VecCons 'True N5.N28) => Finger @Nat.N6 n
pattern N60 = FingerCons SingTrue N5.N28

pattern N61 :: () => (n ~ 'VecCons 'True N5.N29) => Finger @Nat.N6 n
pattern N61 = FingerCons SingTrue N5.N29

pattern N62 :: () => (n ~ 'VecCons 'True N5.N30) => Finger @Nat.N6 n
pattern N62 = FingerCons SingTrue N5.N30

pattern N63 :: () => (n ~ 'VecCons 'True N5.N31) => Finger @Nat.N6 n
pattern N63 = FingerCons SingTrue N5.N31
