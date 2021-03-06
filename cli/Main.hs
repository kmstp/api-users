{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
module Main
where

import Common.Params
import Common.Serialization
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import qualified Data.Text as T
import Database.DSL
import qualified Database.Models as DM
import Database.Queries
import Database.Selda
import Database.Selda.Generic as G
import Options.Generic
import qualified Prelude as P
import Protolude

deriving instance Generic Commands
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
eval (GetPeopleUnderAge age) =
  liftIO . print . fmap (encode . serialize) =<< getPeopleUnderAge age
-- TODO: define eval for CreateUser
--eval (CreateUser n a p) =
  --let up = UserParams n a p


createUsers = tryCreateTable (G.gen DM.users)
createAddresses = tryCreateTable (G.gen DM.addresses)
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
  insert_ (G.gen DM.addresses) (G.toRels DM.sampleAddresses)

