{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Models.User
where

import Aggregate
import Data.Aeson
import Data.Char
import Data.Data
import Data.Either
import Data.Maybe
import Data.Semigroup ((<>))
import qualified Data.Text as T
import GHC.Generics
import Models.Types
import Repository as Repo
import Safe
import Test.Hspec


import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Reader.Class

---------------------------- model ------------------------------------


type Name = T.Text
type Age = Int
type Occupation = T.Text
type MinNameLength = Int
type MinAge = Int

newtype UserId = UserId T.Text deriving (Generic, Eq, Show, Data, ToJSON, FromJSON, Read)

data User =
  User { _userId :: UserId, _name :: Name, _email :: Email, _age :: Age, _occupation :: Occupation }
  deriving (Show, Data, Eq)

instance AggregateId UserId where
  textAggregateId (UserId cid) = cid


--------------------------- aggregate ---------------------------------

instance Aggregate User where
  type Id User = UserId

  data Command User = ChangeAge UserId Age
                    | ChangeName UserId Name
                    deriving (Show, Read)

  data Event User = NameChanged UserId Name
                  | AgeChanged UserId Age
                    deriving (Generic, Show, Data, Eq)

  data Error User = AgeMustBeLargerThan UserId MinAge
                  | NameLengthMustBeLargerThan UserId MinNameLength
                    deriving (Show)

  aggregateId = _userId


  s `execute` ChangeName uid name = NameChanged
    <$> pure uid
    <*> validate (checkNameLength s) (NameLengthMustBeLargerThan uid 3) name

  s `execute` ChangeAge uid age = AgeChanged
    <$> pure uid
    <*> validate (checkAge s) (AgeMustBeLargerThan uid 10) age

  s `apply` NameChanged uid name_ = s { _name = name_}
  s `apply` AgeChanged uid age_ = s { _age = age_ }

  new aid = User aid "" "" 10 ""

-------------------------- validation ---------------------------------

checkNameLength :: User -> Name -> Bool
checkNameLength u n = T.length n >= 3

checkAge :: User -> Age -> Bool
checkAge u a = a >= 10

-------------------------- serialization ---------------------------------

instance ToJSON (Event User) where
  toJSON (NameChanged uid nme)
    = object [ "eventType" .= toJSON ("NameChanged" :: T.Text)
              , "data" .= object [
                  "streamId" .= textAggregateId uid
                , "name" .= nme
                ]
              ]
  toJSON (AgeChanged uid age)
    = object [ "eventType" .= toJSON ("AgeChanged" :: T.Text)
              , "data" .= object [
                  "streamId" .= textAggregateId uid
                , "age" .= age
                ]
              ]



-------------------------- Testing ---------------------------------

sampleId_ = T.pack "user-10"

sampleUserId_ :: UserId
sampleUserId_ = UserId sampleId_

sampleUser_ :: User
sampleUser_ = new sampleUserId_

sampleName_ = "Truong Van Hoa 30"
sampleAge_ = 29
sampleCommand_ :: Command User
sampleCommand_ = ChangeName sampleUserId_ sampleName_

test1 = sampleUser_ `handle` sampleCommand_

defaultHandleValue = (sampleUser_, NameChanged sampleUserId_ "")

--loadUser_ :: Repo.Connection -> UserId -> IO User

{-
exeCmd command aid = do
  input_ <- ActionInput <$> pure defaultCredentials <*> getConnection
  flip runReaderT input_ $ do
    u :: User <- loadAggregate aid
    liftIO . print $ u
    let res = u `handle` command
    case res of
      Left err -> liftIO $ print err
      Right (st, ev) -> do
        saveAggregate st ev
        u :: User <- loadAggregate aid
        liftIO . print $ u

test2 = exeCmd sampleCommand_ sampleUserId_
-}



sampleEvent = NameChanged (UserId "user-123") "Dung1"
test = print "hello"
