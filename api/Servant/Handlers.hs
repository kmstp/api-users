{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Servant.Handlers(
  staticHandlers
, serverHandlers
, graphQLHandlers
, page404Handlers
)
where

import qualified Common
import Common.SharedAPI (GraphQLAPI, HtmlPage(..), ServerRoutes, StaticAPI)
import qualified Data.Text as T
import qualified GraphQL
import GraphQL.Query (RootQueryType, rootQuery)
import qualified Lucid
import qualified Miso
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import Protolude
import Servant ((:<|>)(..))
import qualified Servant
import qualified GraphQL.Request as GR

type (<-<) b a = a -> b
infixl 0 <-<

staticHandlers :: Servant.Server StaticAPI
staticHandlers = Servant.serveDirectoryFileServer "api/static"

serverHandlers :: Servant.Server ServerRoutes
serverHandlers = homeServer :<|> flippedServer

graphQLHandlers :: Servant.Server GraphQLAPI
graphQLHandlers = hello
  where hello :: Servant.Handler GraphQL.Response <-< GR.Request
        hello mname = case handle (GR.query mname) of
                Nothing -> GraphQL.interpretAnonymousQuery @RootQueryType (rootQuery "token") "{}"
                Just n  -> do
                  liftIO $ Protolude.putStrLn n
                  GraphQL.interpretAnonymousQuery @RootQueryType (rootQuery "token") n
        handle qname = if T.null qname 
                       then Nothing
                       else Just qname

  {-
  hello mname = return . HelloMessage $ case mname of
          Nothing -> "Hello, anonymous coward"
          Just n  -> "Hello, " <> n

  -}
-- Alternative type:
-- Servant.Server (ToServerRoutes Common.Home HtmlPage Common.Action)
-- Handles the route for the home page, rendering Common.homeView.
homeServer :: Servant.Handler (HtmlPage (Miso.View Common.Action))
homeServer =
    pure $ HtmlPage $
      Common.viewModel $
      Common.initialModel Common.homeLink

-- Alternative type:
-- Servant.Server (ToServerRoutes Common.Flipped HtmlPage Common.Action)
-- Renders the /flipped page.
flippedServer :: Servant.Handler (HtmlPage (Miso.View Common.Action))
flippedServer =
    pure $ HtmlPage $
      Common.viewModel $
      Common.initialModel Common.flippedLink

page404Handlers :: Servant.Tagged Servant.Handler Servant.Application
page404Handlers = Servant.Tagged page404
-- The 404 page is a Wai application because the endpoint is Raw.
-- It just renders the page404View and sends it to the sclient.
page404 :: Wai.Application
page404 _ respond = respond $ Wai.responseLBS
    HTTP.status404 [("Content-Type", "text/html")] $
    Lucid.renderBS $ Lucid.toHtml Common.page404View
