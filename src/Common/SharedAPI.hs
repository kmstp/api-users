{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Common.SharedAPI(
  GraphQLAPI
, HtmlPage(..)
, ServerRoutes
, StaticAPI
, ServerAPI
, IsoAPI
)
where
import Clay
import qualified Common
import Common.Clay
import Common.GraphQLAPI
import Common.IsoAPI
import qualified Data.Text as T
import qualified Lucid as L
import qualified Lucid.Base as L
import qualified Miso
import Protolude
import Servant.API
    ((:<|>)(..), (:>), AuthProtect, Get, JSON, Post, Raw, ReqBody)
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
            L.link_ [L.rel_ "stylesheet", L.type_ "text/css", L.href_ "/static/styles.css"]

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
       CssAPI
  :<|> StaticAPI
  :<|> GraphQLAPI
  :<|> IsoAPI
  :<|> (ServerRoutes
  :<|> Raw) -- This will show the 404 page for any unknown route

type CssAPI = "static" :> "styles.css" :> Get '[CSS] Css
type StaticAPI = "static" :> Raw
{-
type GraphQLAPI = "graphql"
  :> Servant.QueryParam "query" T.Text
  :> Servant.Get '[Servant.JSON] G.Response
-}
{-
type GraphQLAPI = "graphql"
  :> AuthProtect "cookie-auth"
  :> ReqBody '[JSON] GR.Request
  :> Post '[JSON] G.Response

-}
