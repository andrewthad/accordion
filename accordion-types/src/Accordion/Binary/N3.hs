{-# language DataKinds #-}
{-# language PolyKinds #-}

module Accordion.Binary.N3
  ( N0
  , N1
  , N2
  , N3
  , N4
  , N5
  , N6
  , N7
  ) where

import Accordion.Types (Vec(..))
import qualified Accordion.Binary.N2 as N2

type N0 = 'VecCons 'False N2.N0
type N1 = 'VecCons 'False N2.N1
type N2 = 'VecCons 'False N2.N2
type N3 = 'VecCons 'False N2.N3
type N4 = 'VecCons 'True N2.N0
type N5 = 'VecCons 'True N2.N1
type N6 = 'VecCons 'True N2.N2
type N7 = 'VecCons 'True N2.N3
