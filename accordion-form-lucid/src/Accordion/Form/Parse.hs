{-# language OverloadedStrings #-}

module Accordion.Form.Parse
  ( int
  , char
  , bool
  ) where

import Data.Text (Text)
import Lucid (Html) 

int :: Text -> Either (Html ()) Int
int _ = Left "no parsers yet"
char :: Text -> Either (Html ()) Char
char _ = Left "no parsers yet"
bool :: Text -> Either (Html ()) Bool
bool _ = Left "no parsers yet"
