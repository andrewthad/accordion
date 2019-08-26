{-# language DataKinds #-}
{-# language PolyKinds #-}

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
  ) where

import Accordion.Types (Vec(..))
import qualified Accordion.Binary.N3 as N3

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
