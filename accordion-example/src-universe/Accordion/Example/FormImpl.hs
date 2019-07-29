{-# language GADTs #-}
{-# language OverloadedStrings #-}

module Accordion.Example.FormImpl
  ( Html
  , input
  ) where

import Accordion.Types (Finger)
import Accordion.Example.Universe (Height,showIndexField)
import Accordion.Example.Types (SingField(..),Interpret,Ground,unindex,indexRoundTrip)
import Accordion.Types.Form (Form(..),Encode(..))
import Data.Type.Equality ((:~:)(Refl))
import Data.Text (Text)
import Lucid hiding (Html)
import qualified Accordion.Form.Lucid as F
import qualified Accordion.Form.Parse as FP
import qualified Lucid as L

type Html = L.Html ()

input :: Finger Height v -> Form Html (Ground (Interpret v))
input v = case indexRoundTrip field of
  Refl -> case field of
    SingAlive -> Form (wrap "Alive" F.radioYesNo) FP.bool
    SingLetter -> Form (wrap "Letter" F.char) FP.char
    SingAge -> Form (wrap "Age" F.int) FP.int
    SingHealth -> Form (wrap "Health" F.int) FP.int
  where
  field = unindex v
  wrap :: Text -> Encode Html x -> Encode Html x
  wrap label (Encode f) = Encode $ \r name ->
    div_ [class_ "field"] $ do
      label_ [class_ "label"] $ do
        toHtml label
      f r name
