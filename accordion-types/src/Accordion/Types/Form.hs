module Accordion.Types.Form
  ( Form(..)
  , Encode(..)
  , Result(..)
  ) where

import Data.Text (Text)

data Form w a = Form
  (Encode w a)
  -- Encode a field.
  (Text -> Either w a)
  -- Decode a field.

newtype Encode w a = Encode
  (Result w a -> Text -> w)
  -- Render the form input, possibly including an error.
  -- The second argument is a field name.

data Result w a
  = ResultSuccess a
  | ResultMissing
  | ResultFailure Text w


