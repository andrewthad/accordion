{-# language DataKinds #-}

module Accordion.Nat
  ( N0
  , N1
  , N2
  , N3
  , N4
  , N5
  ) where

import Accordion.Types (Nat(Succ,Zero))

type N0 = 'Zero
type N1 = 'Succ N0
type N2 = 'Succ N1
type N3 = 'Succ N2
type N4 = 'Succ N3
type N5 = 'Succ N4

