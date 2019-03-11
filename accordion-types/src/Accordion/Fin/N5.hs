{-# language DataKinds #-}
{-# language KindSignatures #-}

module Accordion.Fin.N5
  ( F0
  , F1
  , F2
  , F3
  , F4
  , F5
  ) where

import Accordion.Types (Fin(FinZero,FinSucc))
import Accordion.Nat (N5)

type F0 = ('FinZero :: Fin N5)
type F1 = ('FinSucc 'FinZero :: Fin N5)
type F2 = ('FinSucc ('FinSucc 'FinZero) :: Fin N5)
type F3 = ('FinSucc ('FinSucc ('FinSucc 'FinZero)) :: Fin N5)
type F4 = ('FinSucc ('FinSucc ('FinSucc ('FinSucc 'FinZero))) :: Fin N5)
type F5 = ('FinSucc ('FinSucc ('FinSucc ('FinSucc ('FinSucc 'FinZero)))) :: Fin N5)
