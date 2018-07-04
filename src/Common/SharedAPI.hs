{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Common.SharedAPI
where
import qualified Common
import qualified Data.Text as T
import qualified GraphQL as G
import qualified Lucid as L
import qualified Lucid.Base as L
import qualified Miso
import Protolude
import Servant ((:<|>)(..), (:>), Get, JSON, Raw, ReqBody, Post)
import Servant.Client.Core
import qualified GraphQL.Request as GR

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

type StaticAPI = "static" :> Servant.Raw
{-
type GraphQLAPI = "graphql"
  :> Servant.QueryParam "query" T.Text
  :> Servant.Get '[Servant.JSON] G.Response
-}

type GraphQLAPI = "graphql"
  :> Servant.ReqBody '[Servant.JSON] GR.Request 
  :> Servant.Post '[Servant.JSON] G.Response

