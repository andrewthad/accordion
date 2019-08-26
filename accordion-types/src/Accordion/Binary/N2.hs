{-# language DataKinds #-}
{-# language PolyKinds #-}

module Accordion.Binary.N2
  ( N0
  , N1
  , N2
  , N3
  ) where

import Accordion.Types (Vec(..))
import qualified Accordion.Binary.N1 as N1

type N0 = 'VecCons 'False N1.N0
type N1 = 'VecCons 'False N1.N1
type N2 = 'VecCons 'True N1.N0
type N3 = 'VecCons 'True N1.N1


