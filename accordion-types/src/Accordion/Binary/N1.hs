{-# language DataKinds #-}
{-# language PolyKinds #-}

module Accordion.Binary.N1
  ( N0
  , N1
  ) where

import Accordion.Types (Vec(..))
import qualified Accordion.Binary.N0 as N0

type N0 = 'VecCons 'False N0.N0
type N1 = 'VecCons 'True N0.N0

