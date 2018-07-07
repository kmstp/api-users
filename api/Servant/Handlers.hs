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
import Common.SharedAPI (GraphQLAPI, HtmlPage(..), ServerRoutes, StaticAPI)
import qualified Data.Aeson as A
import Data.Aeson.QQ (aesonQQ)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Data.Typeable
import qualified GraphQL
import qualified GraphQL.Internal.Execution as GIE
import qualified GraphQL.Internal.Schema as GIS
import qualified GraphQL.Internal.Syntax.AST as GSA
import qualified GraphQL.Internal.Validation as GIV
import GraphQL.Query (HelloType, RootQueryType, rootQuery)
import qualified GraphQL.Request as GR
import GraphQL.Value (FromValue(..), ToValue(..), makeName, variableValueToAST)
import qualified Lucid
import qualified Miso
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import qualified Prelude as P
import Protolude
import Servant ((:<|>)(..))
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

sampleQuery = do
      let query_ = "query ($whoVar: String!) {greeting(who: $whoVar)}"
      schema <- GraphQL.makeSchema @RootQueryType
      GraphQL.compileQuery schema query_
      --Just z = decodeVariables variables_
  --Protolude.print $
  --  (fmap extractVariableDefinitionsFromOperation . extractValues =<< extractOperations)  cq_
  --Protolude.print $ typeOf $ fmap extractVariableDefinitionsFromOperation . extractValues
  --Protolude.print $ A.encode [aesonQQ| {whoVar: #{val_}} |]

extractOperations :: [GIV.Operations a] <-< GIV.QueryDocument a
extractOperations (GIV.MultipleOperations t)  =  [t]
extractOperations _ = []

extractOperation :: [GIV.Operation a] <-< GIV.QueryDocument a
extractOperation (GIV.LoneAnonymousOperation t) = [t]
extractOperation _ = []

extractValues :: [GIV.Operation a] <-< GIV.Operations a
extractValues = Map.elems

extractVariableDefinitionsFromOperation :: GIV.VariableDefinitions <-< GIV.Operation a
extractVariableDefinitionsFromOperation (GIV.Query x _ _) = x
extractVariableDefinitionsFromOperation (GIV.Mutation x _ _) = x

multipleOperations :: Bool <-< GIV.QueryDocument a
multipleOperations (GIV.MultipleOperations _)  =  True
multipleOperations _  =  False

{-@ type Zero = {v :Int | v == 0} @-}
{-@ type NonZero = {v:Int | v /= 0} @-}
{-@ type Nat = { v: Int | v >= 0} @-}
data Interval  = I
  { _from :: Int
  , _to   :: Int
  } deriving (Show)
{-@ data Interval = I
       { _from :: Nat
       , _to   :: {v: Nat | _from < v }
    }
@-}

{-@
measure len :: [a] -> Int
len [] = 0
len (x:xs) = 1 + len(xs)
@-}

{-@
head1 :: {xs : [a] | len xs >= 1} -> a
@-}

