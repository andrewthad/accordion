{-# language DataKinds #-}

module Accordion.Nat
  ( -- Types
    N0
  , N1
  , N2
  , N3
  , N4
  , N5
    -- Singletons
  , n0
  , n1
  , n2
  , n3
  ) where

import Accordion.Types (Nat(Succ,Zero),SingNat(SingZero,SingSucc))

type N0 = 'Zero
type N1 = 'Succ N0
type N2 = 'Succ N1
type N3 = 'Succ N2
type N4 = 'Succ N3
type N5 = 'Succ N4

n0 :: SingNat N0
n0 = SingZero

n1 :: SingNat N1
n1 = SingSucc n0

n2 :: SingNat N2
n2 = SingSucc n1

n3 :: SingNat N3
n3 = SingSucc n2
