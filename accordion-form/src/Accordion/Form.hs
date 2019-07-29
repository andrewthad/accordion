{-# language DataKinds #-}
{-# language GADTs #-}
{-# language OverloadedStrings #-}

module Accordion.Form
  ( Form(..)
  , Record
  , Result(..)
  , blank
  , blankWith
  , loaded
  , loadedWith
    -- * Template
  , template
  ) where

import Data.Text (Text)
import Data.Functor.Identity (Identity(..))
import Data.Functor.Const (Const(..))
import Accordion.Record (Record(..),Template(..),Interpreted(..))
import Accordion.Universe (Height,singHeight)
import Accordion.Form.Signature (Html,input)
import qualified Accordion.Types.Form as F
import qualified Accordion.Types as A

newtype Form a = Form (F.Form Html a)
newtype Result a = Result (F.Result Html a)

blank :: Record p rs -> Html
blank (Record r) = blankWith
  (Record (A.omnisubset (case template of Template t -> t) r))

blankWith :: Record Form rs -> Html
blankWith (Record t) = A.foldMap oneBlank A.FingerNil t

loaded :: Record Identity rs -> Html
loaded x@(Record r) = loadedWith
  (Record (A.omnisubset (case template of Template t -> t) r))
  x

loadedWith :: Record Form rs -> Record Identity rs -> Html
loadedWith (Record t) (Record v) =
  getConst (A.zipM_ oneLoaded A.FingerNil t v)

oneBlank :: A.Finger Height v -> Interpreted Form v -> Html
oneBlank v (Interpreted (Form (F.Form (F.Encode f) _))) =
  f F.ResultMissing (makeName v)

oneLoaded ::
     A.Finger Height v
  -> Interpreted Form v
  -> Interpreted Identity v
  -> Const Html ()
oneLoaded h
  (Interpreted (Form (F.Form (F.Encode f) _)))
  (Interpreted (Identity val)) =
    Const (f (F.ResultSuccess val) (makeName h))

makeName :: A.Finger Height v -> Text 
makeName x = "h" <> go x where
  go :: A.Finger h w -> Text
  go A.FingerNil = mempty
  go (A.FingerCons b v) =
    (case b of {A.SingTrue -> "-t"; A.SingFalse -> "-f"}) <> go v

template :: Template Form
template = Template
  ( A.omnibuild singHeight
    ( \v -> Interpreted (Form (input v))
    )
  )
