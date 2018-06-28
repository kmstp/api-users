{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}
module Lib
    ( someFunc
    ) where

import           Control.Lens
import           Data.Aeson.Lens           (key, _String)
import qualified Data.ByteString.Lazy      as B
import qualified Data.ByteString.Lazy.UTF8 as BTL
import           Data.Either
import           Data.Text.Lazy.Encoding   as E
import           Data.Tree
import           Data.Typeable
import           MockBody                  (mockBody)
import           Network.Wreq
import           Schemas



url = "http://lib.hpu.edu.vn/handle/123456789/31066?show=full"

type UrlPath = String

type ParseError = String

type HtmlBody = String

parseUrl :: UrlPath -> Either ParseError HtmlBody
parseUrl = undefined


body = BTL.fromString mockBody

writeUrlToFile url filePath = do
  content <- get url
  B.writeFile filePath $ content ^. responseBody

someFunc :: IO ()
someFunc =
  writeUrlToFile url "hpu1.html"

