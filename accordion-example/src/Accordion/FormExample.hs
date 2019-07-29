{-# language DataKinds #-}
{-# language OverloadedStrings #-}

module Accordion.FormExample
  ( module Accordion.Example.Form
  , writeBlank
  , writeLoaded
  ) where

import Accordion.Example.Form
import Data.Functor.Identity (Identity(..))

import Lucid

writeBlank :: Record p rs -> IO ()
writeBlank = renderToFile "example.html" . generateBlank

writeLoaded :: Record Identity rs -> IO ()
writeLoaded = id
  . renderToFile "example.html"
  . wrapHtml
  . loaded

wrapHtml :: Html () -> Html ()
wrapHtml x = do
  html_ $ do
    head_ $ do
      meta_ [charset_ "UTF-8"]
      link_ [rel_ "stylesheet", type_ "text/css", href_ "https://cdnjs.cloudflare.com/ajax/libs/bulma/0.7.5/css/bulma.min.css"]
    body_ $ do
      div_ [class_ "section"] $ do
        div_ [class_ "container"] $ do
          h1_ [class_ "title"] "Example Form"
          form_ x

generateBlank :: Record p rs -> Html ()
generateBlank r = wrapHtml (blank r)
