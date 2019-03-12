module Accordion.Example
  ( module Accordion.Example.Record
  , module Accordion.Example.Types
    -- fields
  , age
  , health
  , letter
  , alive
  ) where

import Accordion.Example.Record
import Accordion.Example.Types
import Accordion.Types (Finger(..),SingBool(..))

age :: Field Age
age = Field (FingerCons SingTrue (FingerCons SingTrue FingerNil))

health :: Field Health
health = Field (FingerCons SingTrue (FingerCons SingFalse FingerNil))

letter :: Field Letter
letter = Field (FingerCons SingFalse (FingerCons SingTrue FingerNil))

alive :: Field Alive
alive = Field (FingerCons SingFalse (FingerCons SingFalse FingerNil))
