{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Main where

import qualified Common
import qualified Data.Aeson as Aeson
import Data.Proxy
import Data.Semigroup ((<>))
import Data.Text as T
import qualified GHC.Generics as GG
import qualified GraphQL as G
import GraphQL.Query
import GraphQL.Wai
import qualified Lucid as L
import qualified Lucid.Base as L
import Miso (View)
import qualified Miso
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Wai
import qualified Network.Wai.Middleware.Gzip as Wai
import qualified Network.Wai.Middleware.RequestLogger as Wai
import Servant ((:<|>)(..), (:>))
import qualified Servant
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
        (    static
        :<|> graphQLHandlers
        :<|> serverHandlers
        :<|> Servant.Tagged page404
        )
  where
    static :: Servant.Server StaticAPI
    static = Servant.serveDirectoryFileServer "api/static"

    serverHandlers :: Servant.Server ServerRoutes
    serverHandlers = homeServer :<|> flippedServer

    graphQLHandlers :: Servant.Server GraphQLAPI
    graphQLHandlers = hello
      where hello :: Maybe T.Text -> Servant.Handler G.Response
            hello mname = case mname of
              Nothing -> G.interpretAnonymousQuery @RootQueryType (rootQuery "token") "{}"
              Just n  -> G.interpretAnonymousQuery @RootQueryType (rootQuery "token") n


      {-
      hello mname = return . HelloMessage $ case mname of
              Nothing -> "Hello, anonymous coward"
              Just n  -> "Hello, " <> n

      -}
    -- Alternative type:
    -- Servant.Server (ToServerRoutes Common.Home HtmlPage Common.Action)
    -- Handles the route for the home page, rendering Common.homeView.
    homeServer :: Servant.Handler (HtmlPage (View Common.Action))
    homeServer =
        pure $ HtmlPage $
          Common.viewModel $
          Common.initialModel Common.homeLink

    -- Alternative type:
    -- Servant.Server (ToServerRoutes Common.Flipped HtmlPage Common.Action)
    -- Renders the /flipped page.
    flippedServer :: Servant.Handler (HtmlPage (View Common.Action))
    flippedServer =
        pure $ HtmlPage $
          Common.viewModel $
          Common.initialModel Common.flippedLink


    -- The 404 page is a Wai application because the endpoint is Raw.
    -- It just renders the page404View and sends it to the client.
    page404 :: Wai.Application
    page404 _ respond = respond $ Wai.responseLBS
        HTTP.status404 [("Content-Type", "text/html")] $
        L.renderBS $ L.toHtml Common.page404View

-- | Represents the top level Html code. Its value represents the body of the
-- page.
newtype HtmlPage a = HtmlPage a
  deriving (Show, Eq)

instance L.ToHtml a => L.ToHtml (HtmlPage a) where
    toHtmlRaw = L.toHtml
    toHtml (HtmlPage x) =
        L.doctypehtml_ $ do
          L.head_ $ do
            L.title_ "Miso isomorphic example"
            L.meta_ [L.charset_ "utf-8"]

            L.with (L.script_ mempty)
              [ L.makeAttribute "src" "/static/app.js"
              , L.makeAttribute "async" mempty
              , L.makeAttribute "defer" mempty
              ]

          L.body_ (L.toHtml x)

-- Converts the ClientRoutes (which are a servant tree of routes leading to
-- some `View action`) to lead to `Get '[Html] (HtmlPage (View Common.Action))`
type ServerRoutes
   = Miso.ToServerRoutes Common.ViewRoutes HtmlPage Common.Action

-- The server serves static files besides the ServerRoutes, among which is the
-- javascript file of the client.
type ServerAPI =
       StaticAPI
  :<|> GraphQLAPI
  :<|> (ServerRoutes
  :<|> Servant.Raw) -- This will show the 404 page for any unknown route

newtype HelloMessage = HelloMessage { msg :: T.Text }
  deriving GG.Generic

instance Aeson.ToJSON HelloMessage

type StaticAPI = "static" :> Servant.Raw
type GraphQLAPI = "graphql"
  :> Servant.QueryParam "query" T.Text
  :> Servant.Get '[Servant.JSON] G.Response

test = print "Hello"
