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
  , encodeRecords
  , encodeOptionals
  , dump
  , dumpOpt
  ) where

import Accordion.Example.Base
import Accordion.Example.Universe
import Accordion.Example.Types
import Accordion.Example.Json (encodeRecords,encodeOptionals)
import Data.Char (chr)
import qualified Accordion.Types as A
import qualified GHC.Exts as E

health :: A.Finger FieldHeight (Index 'Health)
health = index SingHealth

age :: A.Finger FieldHeight (Index 'Age)
age = index SingAge

alive :: A.Finger FieldHeight (Index 'Alive)
alive = index SingAlive

dump :: Records m -> IO ()
dump = mapM_ (putStrLn . map (chr . fromIntegral) . E.toList) . encodeRecords 5

dumpOpt :: Optionals m -> IO ()
dumpOpt =
  mapM_ (putStrLn . map (chr . fromIntegral) . E.toList) . encodeOptionals 5


