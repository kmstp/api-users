module Database.DSL where

import Control.Monad.Catch
import Control.Monad.IO.Class (MonadIO)
import qualified Data.Text as T
import Database.Selda
import Database.Selda.PostgreSQL
import Database.Selda.SQLite
import Protolude hiding (on)

type TblName = T.Text

data Commands =
    CreateTable TblName
  | DropTable TblName
  | Setup
  | Teardown
  | Seed
  | GetPeopleOfAge Int
  | GetPeopleUnderAge Int
  | CreateUser Text Int (Maybe Text)
  deriving Show

data Backend = Postgres PGConnectInfo | Sqlite FilePath

defaultBackend = Postgres $ "blog1" `on` "localhost" `auth` ("macbookpro", "Dung123#")

defaultConnInfo = "blog1" `on` "localhost" `auth` ("macbookpro", "Dung123#")

withDefaultBackend action = withPostgreSQL defaultConnInfo action

runCommand (Postgres connInfo) command =
  withPostgreSQL connInfo $ do
    setLocalCache 1000
    command

runCommand (Sqlite connInfo) command =
  withSQLite connInfo command
