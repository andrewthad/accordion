{-# language DataKinds #-}
{-# language KindSignatures #-}

-- | Evidence that 2 is greater than or equal to the numbers
--   less than it.
module Accordion.Gte.N2
  ( G0
  , G1
  , G2
  ) where

import Accordion.Types (Gte(GteEq,GteGt))
import Accordion.Nat (N2,N1,N0)

type G0 = ('GteGt ('GteGt 'GteEq) :: Gte N2 N0)
type G1 = ('GteGt 'GteEq :: Gte N2 N1)
type G2 = ('GteEq :: Gte N2 N2)

