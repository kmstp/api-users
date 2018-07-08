{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module Main
where

import Common.Serialization
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import qualified Data.Text as T
import qualified Database.Models as DM
import Database.Queries
import Database.Selda
import qualified Database.Selda.Generic as G
import Database.Selda.PostgreSQL
import Database.Selda.SQLite
import Options.Generic
import qualified Prelude as P
import Protolude hiding ((:*:), on)

type TblName = T.Text

data Backend = Postgres PGConnectInfo | Sqlite FilePath

defaultBackend = Postgres $ "blog1" `on` "localhost" `auth` ("macbookpro", "Dung123#")

runCommand (Postgres connInfo) command =
  withPostgreSQL connInfo $ do
    setLocalCache 1000
    command

runCommand (Sqlite connInfo) command =
  withSQLite connInfo command

data Commands =
    CreateTable TblName
  | DropTable TblName
  | Setup
  | Teardown
  | Seed
  | GetPeopleOfAge Int
  | GetPeopleUnderAge Int
  deriving (Generic, Show)

instance ParseRecord Commands

main = do
    x :: Commands <- getRecord "Test program"
    print x
    runCommand defaultBackend (eval x)

eval :: Commands -> SeldaM ()
eval (CreateTable tn) =
  case tn of
    "users"    -> createUsers
    "addresses" -> createAddresses
    _           -> liftIO $ print "No table declared"
eval (DropTable tn) =
  case tn of
    "users"    -> dropUsers
    "addresses" -> dropAddresses
    _           -> liftIO $ print "No table declared"
eval Setup = setup
eval Teardown = teardown
eval Seed = seed
eval (GetPeopleOfAge age) = do
  usrs <- getPeopleOfAge age
  liftIO $ print usrs
eval (GetPeopleUnderAge age) = -- do
  --usrs <- getPeopleUnderAge age
  liftIO . print . fmap (encode . serialize) =<< getPeopleUnderAge age

createUsers = createTable (G.gen DM.users)
createAddresses = createTable (G.gen DM.addresses)
dropUsers = tryDropTable (G.gen DM.users)
dropAddresses = tryDropTable (G.gen DM.addresses)

setup = do
  createUsers
  createAddresses

teardown = do
  dropUsers
  dropAddresses


seed = do
  insert_ (G.gen DM.users) (G.toRels DM.sampleUsers)
--    , def :*: "Miyu"      :*: 10  :*: Nothing
  insert_ (G.gen DM.addresses)
    [ def :*: "Link"      :*: "Kakariko"
    , def :*: "Kobayashi" :*: "Tokyo"
    , def :*: "Miyu"      :*: "Fuyukishi"
    ]
