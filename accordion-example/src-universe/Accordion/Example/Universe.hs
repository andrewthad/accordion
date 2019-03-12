{-# language DataKinds #-}
{-# language GADTSyntax #-}
{-# language KindSignatures #-}
{-# language TypeFamilies #-}

module Accordion.Example.Universe
  ( Height
  , Interpret
  , interpretShow
  ) where

import Accordion.Types (Nat(..),Vec(..),Shown(..),Omnitree(..))
import Accordion.Nat (N2)
import Accordion.Example.Types (Ground,Interpret(..))
import Data.Kind (Type)

type Height = N2

interpretShow :: Omnitree Height 'Zero (Shown (Vec Height Bool) Interpret) 'VecNil
interpretShow = OmnitreeBranch
  (OmnitreeBranch
    (OmnitreeLeaf (Shown (show . getInterpret)))
    (OmnitreeLeaf (Shown (show . getInterpret)))
  )
  (OmnitreeBranch
    (OmnitreeLeaf (Shown (show . getInterpret)))
    (OmnitreeLeaf (Shown (show . getInterpret)))
  )
