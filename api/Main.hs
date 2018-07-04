{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Main where

import Common.SharedAPI (ServerAPI)
import Data.Proxy (Proxy(..))
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Wai
import qualified Network.Wai.Middleware.Gzip as Wai
import qualified Network.Wai.Middleware.RequestLogger as Wai
import Protolude
import Servant ((:<|>)(..))
import qualified Servant
import Servant.Handlers
    (graphQLHandlers, page404Handlers, serverHandlers, staticHandlers)
import qualified System.IO as IO

main :: IO ()
main = do
    IO.hPutStrLn IO.stderr "Running on port 3003..."

    Wai.run 3003 $ Wai.logStdout $ compress app
  where
    compress :: Wai.Middleware
    compress = Wai.gzip Wai.def { Wai.gzipFiles = Wai.GzipCompress }

app :: Wai.Application
app =
    Servant.serve (Proxy @ServerAPI)
        (    staticHandlers
        :<|> graphQLHandlers
        :<|> serverHandlers
        :<|> page404Handlers
        )
