{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module API where

import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.Trans.Except  (throwE)
import           Data.Int                    (Int64)
import           Data.Proxy                  (Proxy (..))
import           Database.Persist            (Entity, Key)
import           Database.Persist.Postgresql (ConnectionString)
import           Network.Wai.Handler.Warp    (defaultSettings, runSettings,
                                              setLogger, setPort)
import           Network.Wai.Logger          (withStdoutLogger)
import           Servant.API
import           Servant.Client
import           Servant.Server

import           Data.Typeable
import           Database                    (createUserPG,
                                              fetchPostgresConnection,
                                              fetchUserPG)
import           Schemas                     (User)

type UsersAPI =
       "users" :> Capture "userid" Int64 :> Get '[JSON] User
  :<|> "users" :> ReqBody '[JSON] User :> Post '[JSON] Int64

usersAPI :: Proxy UsersAPI
usersAPI = Proxy :: Proxy UsersAPI

fetchUsersHandler :: ConnectionString -> Int64 -> Handler User
fetchUsersHandler connString uid = do
  maybeUser <- liftIO $ fetchUserPG connString uid
  case maybeUser of
    Just user -> return user
    Nothing -> Handler
               $ throwE
               $ err401 { errBody = "Could not find user with that ID" }

createUserHandler :: ConnectionString -> User -> Handler Int64
createUserHandler connString user = liftIO $ createUserPG connString user

usersServer :: ConnectionString -> Server UsersAPI
usersServer connString =
  fetchUsersHandler connString :<|>
  createUserHandler connString

app :: ConnectionString -> Application
app  connString = serve usersAPI (usersServer connString)

runServer :: IO ()
runServer = do
  connString <- fetchPostgresConnection
  withStdoutLogger $ \aplogger -> do
    let settings = setPort 8000 $ setLogger aplogger defaultSettings
    runSettings settings (app connString)
