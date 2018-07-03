
module Middlewares.Logger
where

import Data.ByteString as B
import Data.ByteString.Char8 as BC
import Data.Monoid (mconcat)
import Data.Text.Encoding (encodeUtf8)
import Network.Wai

logAllMiddleware :: Application -> Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
logAllMiddleware app req respond = do
  let paths = pathInfo req
  bdy <- requestBody req
  BC.putStrLn bdy
  BC.putStrLn . encodeUtf8 $ mconcat paths
  app req respond
