{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
--{-# LANGUAGE TypeSynonymInstances #-}
module API where

import Clay hiding (link, type_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (throwE)
import qualified Data.Aeson as Aeson
import Data.Int (Int64)
import Data.Proxy (Proxy(..))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Lazy.Encoding
import Data.Typeable
import Database (createUserPG, fetchPostgresConnection, fetchUserPG)
import Database.Persist (Entity, Key)
import Database.Persist.Postgresql (ConnectionString)
import GraphQL
import GraphQL.Query
import GraphQL.Value.ToValue (toValue)
import Lucid
import Network.Wai.Handler.Warp
    (defaultSettings, runSettings, setLogger, setPort)
import Network.Wai.Logger (withStdoutLogger)
import Network.Wai.Middleware.Gzip
import Schemas (User)
import Servant.API
import Servant.Client
import Servant.CSS.Clay
import Servant.Docs
import Servant.HTML.Lucid
import Servant.Server
import Servant.Utils.StaticFiles (serveDirectoryFileServer)

cssStyle :: Css
cssStyle = body ? background white

type UsersAPI =
  Get '[HTML] (Html ())
  :<|> "graphql" :> ReqBody '[PlainText] T.Text :> Post '[JSON] GraphQL.Response
  :<|> "api" :> "v1" :> "users" :> Capture "userid" Int64 :> Get '[JSON] User
  :<|> "api" :> "v1" :>"users" :> ReqBody '[JSON] User :> Post '[JSON] Int64
  :<|> "styles.css" :> Get '[CSS] Css
  :<|> "static" :> Raw

usersAPI :: Proxy UsersAPI
usersAPI = Proxy :: Proxy UsersAPI

usersServer :: ConnectionString -> Server UsersAPI
usersServer connString =
  return indexHtmlHandler :<|>
  testGraph :<|>
  fetchUsersHandler connString :<|>
  createUserHandler connString :<|>
  return cssStyle :<|>
  serveDirectoryFileServer "web/static"

fetchUsersHandler :: ConnectionString -> Int64 -> Handler User
fetchUsersHandler connString uid = do
  maybeUser <- liftIO $ fetchUserPG connString uid
  case maybeUser of
    Just user -> return user
    Nothing -> Handler
               $ throwE
               $ err401 { errBody = "Could not find user with that ID" }

createUserHandler :: ConnectionString -> User -> Handler Int64
createUserHandler connString user = do
  newUserId <- liftIO $ createUserPG connString user
  case newUserId of
    Right uid -> return uid
    Left _ -> Handler
              $ throwE
              $ err401 { errBody = "Could not create user with that information" }



pageTemplate :: Html () -> Html ()
pageTemplate contents =
  html_ (do head_ (do title_ "Hello World!"
                      link_ [rel_ "stylesheet",type_ "text/css",href_ "styles.css"]
                      script_ [src_ "static/app.js"] ("" :: T.Text))
            body_ contents)

link :: T.Text -> Html () -> Html ()
link url  = a_ [href_ url]

indexHtmlHandler :: Html ()
indexHtmlHandler =
  pageTemplate
    (do h1_ "Hello!"
        p_ "Hello, Lucid!"
        p_ (do "I love!"
               link "http://haskell.org" "Haskell!")
        div_ [id_ "counter"] "")

app :: ConnectionString -> Application
app  connString = gzip (def { gzipFiles = GzipCompress })
            $ serve usersAPI (usersServer connString)

runServer :: IO ()
runServer = do
  connString <- fetchPostgresConnection
  withStdoutLogger $ \aplogger -> do
    let settings = setPort 8000 $ setLogger aplogger defaultSettings
    runSettings settings (app connString)

testGraph :: T.Text -> Handler GraphQL.Response
testGraph q_ = do
  liftIO $ TIO.putStrLn q_
  interpretAnonymousQuery @RootQueryType (rootQuery "token") q_
    --where query = "{me{name}}"

instance ToCapture (Capture "userid" Int64) where
  toCapture _ = DocCapture "userid" "userid of the person to get"

instance ToSample (Html ()) where
  toSamples _ = []

instance ToSample Css where
  toSamples _ = []

instance ToSample T.Text where
  toSamples _ = []

instance ToSample GraphQL.Response where
  toSamples _ = []

instance ToSample User where
  toSamples _ = []

instance ToSample Int64 where
  toSamples _ = [("ok", 1), ("not ok", 2)]

greetDocs :: API
greetDocs = docs usersAPI

genDocs :: IO ()
genDocs = putStrLn $ markdown greetDocs
