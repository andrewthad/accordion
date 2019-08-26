{-# language DataKinds #-}

module Accordion.Nat
  ( -- Types
    N0
  , N1
  , N2
  , N3
  , N4
  , N5
  , N6
  , N7
    -- Singletons
  , n0
  , n1
  , n2
  , n3
  , n4
  , n5
  , n6
  , n7
  ) where

import Accordion.Types (Nat(Succ,Zero),SingNat(SingZero,SingSucc))

type N0 = 'Zero
type N1 = 'Succ N0
type N2 = 'Succ N1
type N3 = 'Succ N2
type N4 = 'Succ N3
type N5 = 'Succ N4
type N6 = 'Succ N5
type N7 = 'Succ N6

n0 :: SingNat N0
n0 = SingZero

n1 :: SingNat N1
n1 = SingSucc n0

n2 :: SingNat N2
n2 = SingSucc n1

n3 :: SingNat N3
n3 = SingSucc n2

n4 :: SingNat N4
n4 = SingSucc n3

n5 :: SingNat N5
n5 = SingSucc n4

n6 :: SingNat N6
n6 = SingSucc n5

n7 :: SingNat N7
n7 = SingSucc n6
