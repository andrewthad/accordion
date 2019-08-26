{-# language DataKinds #-}
{-# language KindSignatures #-}

module Accordion.Binary.N0
  ( N0
  ) where

import Accordion.Types (Vec(..),Nat(Zero))

type N0 = ('VecNil :: Vec 'Zero Bool)
