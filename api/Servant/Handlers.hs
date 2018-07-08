{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-@ LIQUID "--short-names" @-}
{-@ LIQUID "--exact-data-con" @-}
{-@ LIQUID "--no-adt" @-}
{-@ LIQUID "--prune-unsorted" @-}
{-@ LIQUID "--higherorder" @-}
{-@ LIQUID "--no-termination" @-}
module Servant.Handlers
{-(
  staticHandlers
, serverHandlers
, graphQLHandlers
, page404Handlers
, testHandlers
)
-}

where
import qualified Auth.Auth as AA
import qualified Common
import Common.SharedAPI
    (GraphQLAPI, HtmlPage(..), ServerAPI, ServerRoutes, StaticAPI)
import qualified Data.Aeson as A
import Data.Aeson.QQ (aesonQQ)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.Map as Map
import Data.Proxy
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Data.Typeable
import qualified GraphQL
import GraphQL.API
import qualified GraphQL.Internal.Execution as GIE
import qualified GraphQL.Internal.Schema as GIS
import qualified GraphQL.Internal.Syntax.AST as GSA
import qualified GraphQL.Internal.Validation as GIV
import GraphQL.Query (HelloType, RootQueryType, rootQuery)
import qualified GraphQL.Request as GR
import GraphQL.Resolver
import GraphQL.Value hiding (Object)
import GraphQL.Value.ToValue
import qualified Lucid
import qualified Miso
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import qualified Prelude as P
import Protolude
import Servant ((:<|>)(..), safeLink)
import qualified Servant

type (<-<) b a = a -> b
infixl 0 <-<

staticHandlers :: Servant.Server StaticAPI
staticHandlers = Servant.serveDirectoryFileServer "api/static"


serverHandlers :: Servant.Server ServerRoutes
serverHandlers = homeServer :<|> flippedServer

graphQLHandlers :: Servant.Server GraphQLAPI
graphQLHandlers = hello
  where
        hello ac mname = case handle (GR.query mname) of
          Nothing -> GraphQL.interpretAnonymousQuery @RootQueryType (rootQuery "token") "{}"
          Just n  -> do
            liftIO $ Protolude.print ac
            liftIO $ Protolude.putStrLn n
            liftIO $ Protolude.print (safeLink (Proxy::Proxy ServerAPI) (Proxy::Proxy GraphQLAPI))
            let Right schema = GraphQL.makeSchema @RootQueryType
                -- Right query = GraphQL.compileQuery schema n
                query = n
                Right name = makeName "myQuery"
                vars = Map.singleton (GSA.Variable "who") $ toValue @Text "Truong Dung"
            liftIO $ print vars
            liftIO $ print $ decodeVariables (GR.variables mname)
            --GraphQL.interpretAnonymousQuery @RootQueryType (rootQuery "token") n
            GraphQL.interpretQuery @RootQueryType (rootQuery "token") query (Just name) vars
        handle qname = if T.null qname
                       then Nothing
                       else Just qname

decodeVariables :: Maybe A.Object <-< TL.Text
decodeVariables vars = A.decode (TLE.encodeUtf8 vars)

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

testHandlers :: IO ()
testHandlers = do
  let query_ = "query ($whoVar: String!) {greeting(who: $whoVar)}"
      val_ = "test" :: T.Text
      --variables_ = A.encode [aesonQQ| {who: #{val_}]} |] -- "{ \"who\": \"test\" }"
      Right schema = GraphQL.makeSchema @RootQueryType
      Right tn = makeName "UserType"
      t_ = GIS.lookupType schema tn
      Right cq_ = GraphQL.compileQuery schema query_
      Right n = makeName "test"
      x = GSA.Variable n
      y = toValue @Int32 10
  return ()

type User1 = Object "User" '[] '[Field "name" Text]
type Query1 = Object "Query" '[] '[Field "me" User1]

user1 :: Handler IO User1
user1 = pure name
  where
    name = pure "Mort"

query1 :: Handler IO Query1
query1 = pure user1

sampleQuery :: IO GraphQL.Response
sampleQuery = GraphQL.interpretAnonymousQuery @Query1 query1 "{ me { name} }"
      --Just z = decodeVariables variables_
  --Protolude.print $
  --  (fmap extractVariableDefinitionsFromOperation . extractValues =<< extractOperations)  cq_
  --Protolude.print $ typeOf $ fmap extractVariableDefinitionsFromOperation . extractValues
  --Protolude.print $ A.encode [aesonQQ| {whoVar: #{val_}} |]
