{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module Main
where

import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import qualified Database.Models as DM
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
  deriving (Generic, Show)

instance ParseRecord Commands

main = do
    x :: Commands <- getRecord "Test program"
    print x
    runCommand defaultBackend (eval x)

eval :: Commands -> SeldaM ()
eval (CreateTable tn) =
  case tn of
    "people"    -> createPeople
    "addresses" -> createAddresses
    _           -> liftIO $ print "No table declared"
eval (DropTable tn) =
  case tn of
    "people"    -> dropPeople
    "addresses" -> dropAddresses
    _           -> liftIO $ print "No table declared"
eval Setup = setup
eval Teardown = teardown
eval Seed = seed


createPeople = createTable (G.gen DM.people)
createAddresses = createTable (G.gen DM.addresses)
dropPeople = tryDropTable (G.gen DM.people)
dropAddresses = tryDropTable (G.gen DM.addresses)

setup = do
  createPeople
  createAddresses

teardown = do
  dropPeople
  dropAddresses

seed = do
  insert_ (G.gen DM.people)
    [ def :*: "Link"      :*: 125 :*: Just "horse"
    , def :*: "Velvet"    :*: 19  :*: Nothing
    , def :*: "Kobayashi" :*: 23  :*: Just "dragon"
    , def :*: "Miyu"      :*: 10  :*: Nothing
    ]
  insert_ (G.gen DM.addresses)
    [ def :*: "Link"      :*: "Kakariko"
    , def :*: "Kobayashi" :*: "Tokyo"
    , def :*: "Miyu"      :*: "Fuyukishi"
    ]
