{-# language OverloadedStrings #-}

module Accordion.Form.Lucid
  ( int
  , char
  , radioYesNo
  ) where

import Lucid
import Accordion.Types.Form (Encode(..),Result(..))
import Data.Text (Text)
import qualified Data.Text as T

int :: Encode (Html ()) Int
int = Encode $ \r name -> case r of
  ResultSuccess i -> intSuccess name i
  ResultMissing -> generalMissing "number" name
  ResultFailure val msg -> generalFailure "number" name val msg

char :: Encode (Html ()) Char
char = Encode $ \r name -> case r of
  ResultSuccess i -> charSuccess name i
  ResultMissing -> generalMissing "text" name
  ResultFailure val msg -> generalFailure "text" name val msg

-- We add the extra space before the labels to prevent
-- them from butting up against the radio buttons.
radioYesNo :: Encode (Html ()) Bool
radioYesNo = Encode $ \r name -> case r of
  ResultSuccess i -> binaryRadioSuccess " Yes" " No" name i
  ResultMissing -> binaryRadioMissing " Yes" " No" name
  ResultFailure _ msg -> binaryRadioFailure " Yes" " No" name msg

charSuccess :: Text -> Char -> Html ()
charSuccess n val = div_ [class_ "control"] $ do
  input_ [class_ "input", type_ "text", name_ n, value_ (T.singleton val)]

intSuccess :: Text -> Int -> Html ()
intSuccess n val = div_ [class_ "control"] $ do
  input_ [class_ "input", type_ "number", name_ n, value_ (T.pack (show val))]

generalMissing :: Text -> Text -> Html ()
generalMissing typ n = div_ [class_ "control"] $ do
  input_ [class_ "input", type_ typ, name_ n]

generalFailure :: Text -> Text -> Text -> Html () -> Html ()
generalFailure typ n val msg = do
  div_ [class_ "control has-icons-right"] $ do
    input_ [class_ "input is-danger", type_ typ, name_ n, value_ val]
    span_ [class_ "icon is-small is-right"] $ do
      i_ [class_ "fas fa-exclamation-triangle"] (pure ())
  p_ [class_ "help is-danger"] msg

binaryRadioSuccess :: Text -> Text -> Text -> Bool -> Html ()
binaryRadioSuccess true false n val =
  binaryRadioCommon true false n (val == True) (val == False)

binaryRadioMissing :: Text -> Text -> Text -> Html ()
binaryRadioMissing true false n =
  binaryRadioCommon true false n False False

binaryRadioCommon :: Text -> Text -> Text -> Bool -> Bool -> Html ()
binaryRadioCommon true false n isTrue isFalse = do
  div_ [class_ "control"] $ do
    let commonAttrs = [name_ n, type_ "radio"]
    label_ [class_ "radio"] $ do
      let attrs = value_ "t" : commonAttrs
      input_ (if isTrue then checked_ : attrs else attrs)
      toHtml true
    label_ [class_ "radio"] $ do
      let attrs = value_ "f" : commonAttrs
      input_ (if isFalse then checked_ : attrs else attrs)
      toHtml false

binaryRadioFailure :: Text -> Text -> Text -> Html () -> Html ()
binaryRadioFailure true false n msg = do
  binaryRadioCommon true false n False False
  p_ [class_ "help is-danger"] msg
