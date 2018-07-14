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
, restHandlers
)
-}

where
import qualified Auth.Auth as AA
import qualified Common
import qualified Common.Serialization as CS
import Common.SharedAPI
    (GraphQLAPI, HtmlPage(..), IsoAPI, ServerAPI, ServerRoutes, StaticAPI)
import Control.Lens
import Control.Monad.Trans.Except (throwE)
import qualified Data.Aeson as A
import Data.Aeson.QQ (aesonQQ)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as C
import Data.HashMap.Lazy as HL
import qualified Data.Map as Map
import Data.Proxy
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Data.Typeable
import qualified Database.DSL as DSL
import qualified Database.Queries as DQ
import qualified GraphQL as G
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
import Servant ((:<|>)(..), err403, errBody, safeLink)
import qualified Servant

type (<-<) b a = a -> b
infixl 0 <-<



staticHandlers :: Servant.Server StaticAPI
staticHandlers = Servant.serveDirectoryFileServer "api/static"

restHandlers :: Servant.Server IsoAPI
restHandlers = homeJSONServer

serverHandlers :: Servant.Server ServerRoutes
serverHandlers = homeServer :<|> flippedServer

graphQLHandlers :: Servant.Server GraphQLAPI
graphQLHandlers = hello1
  where
        hello1 Nothing _ = Servant.Handler $ throwE $ Servant.err401 { Servant.errBody = "Unauthorized" }
        hello1 (Just x) mname | x /= "secret-code" = Servant.Handler $ throwE $ Servant.err401 { Servant.errBody = "Unauthorized" }
        hello1 (Just "secret-code") mname = case handle (GR.query mname) of
          Nothing -> G.interpretAnonymousQuery @RootQueryType (rootQuery "token") "{}"
          Just n  -> do
            liftIO $ Protolude.putStrLn n
            liftIO $ Protolude.print (safeLink (Proxy::Proxy ServerAPI) (Proxy::Proxy GraphQLAPI))
            let Right schema = G.makeSchema @RootQueryType
                -- Right query = GraphQL.compileQuery schema n
                query = n
                Right name = makeName "myQuery"
                Just varMap = decodeVariables (GR.variables mname)
                -- Just who = Map.lookup (T.pack "who") varMap
                vars = Map.singleton (GSA.Variable "who") $ toValue @Text "Dung"
            liftIO $ print  vars
            liftIO $ print varMap
            --GraphQL.interpretAnonymousQuery @RootQueryType (rootQuery "token") n
            G.interpretQuery @RootQueryType (rootQuery "token") query (Just name) vars
        handle qname = if T.null qname
                       then Nothing
                       else Just qname

decodeVariables :: TL.Text -> Maybe (Map Text A.Value)
decodeVariables vars = A.decode (TLE.encodeUtf8 vars)


convertLeft vname = GSA.Variable (Name { unName = vname })
convertRight (String x) = toValue @Text x

convertAeson :: A.Value -> Value
convertAeson = undefined
  {-
  hello mname = return . HelloMessage $ case mname of
          Nothing -> "Hello, anonymous coward"
          Just n  -> "Hello, " <> n

  -}
getHomeModel = do
  users <- liftIO $ DSL.withDefaultBackend $ fmap CS.serialize <$> DQ.getPeopleUnderAge 50
  pure $
    Common.initialModel Common.homeLink
    & Common.model
    .~ (Common.Home $ Common.initialHomeModel & Common.users .~ users)
-- Alternative type:
-- Servant.Server (ToServerRoutes Common.Home HtmlPage Common.Action)
-- Handles the route for the home page, rendering Common.homeView.
homeServer :: Servant.Handler (HtmlPage (Miso.View Common.Action))
homeServer = HtmlPage . Common.viewModel <$> getHomeModel

homeJSONServer :: Servant.Handler Common.Model
homeJSONServer = getHomeModel
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
