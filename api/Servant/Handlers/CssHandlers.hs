{-# LANGUAGE OverloadedStrings #-}
module Servant.Handlers.CssHandlers
where

import Clay
import Protolude
import Servant.Server

headerStyle :: Css
headerStyle = header ?
  do background lightgrey
     paddingTop (em 1.5)

navStyle :: Css
navStyle = nav ?
  do background (Other "#1abc9c")
     ul ? marginTop (px 0) <> paddingTop (px 0) <> listStyleType none
     li ?
        do display inlineBlock
           a ? display inlineBlock <> textDecoration none <> color white <> padding (px 14) (px 16) (px 14) (px 16)


cssHandlers :: Handler Css
cssHandlers =
   return $ headerStyle <> navStyle
