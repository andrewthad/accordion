{-# language DataKinds #-}

module Accordion.Example
  ( Records
  , Record
    -- Create
  , one
  , field
  , union
    -- Fields
  , health
  , age
    -- JSON
  , encode
  , dump
  ) where

import Accordion.Example.Base
import Accordion.Example.Universe
import Accordion.Example.Types
import Accordion.Example.Json (encode)
import Data.Char (chr)
import qualified Accordion.Types as A
import qualified GHC.Exts as E

health :: A.Finger FieldHeight (Index 'Health)
health = index SingHealth

age :: A.Finger FieldHeight (Index 'Age)
age = index SingAge

dump :: Records m -> IO ()
dump = mapM_ (putStrLn . map (chr . fromIntegral) . E.toList) . encode 5


