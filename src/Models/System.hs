{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Models.System
where

import Aggregate
import Control.Monad.IO.Class
import Data.Aeson
import Data.Data
import Data.Either
import Data.Semigroup ((<>))
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import GHC.Generics
import Models.Types
import Models.User (UserId)
import Protolude hiding (handle)
import Repository as Repo
import Test.Hspec
import qualified Text.Email.Validate as EV

---------------------------- model ------------------------------------

newtype SystemId = SystemId T.Text deriving (Generic, Eq, Show, Data, ToJSON, FromJSON, Read)

data System =
  System {
      _systemId :: SystemId,
      _emails   :: Set Email
    }
  deriving (Show, Data, Eq)

instance AggregateId SystemId where
  textAggregateId (SystemId cid) = cid


--------------------------- aggregate ---------------------------------

instance Aggregate System where
  type Id System = SystemId

  data Command System = SignUpUser Email Password UUID
                    deriving (Show, Read)

  data Event System = UserSignedUp Email UUID
                    deriving (Generic, Show, Data, FromJSON, ToJSON, Eq)


  data Error System = EmailMustBeUnique
                    | EmailMustBeValid
                    deriving (Show)

  aggregateId = _systemId


  s `execute` SignUpUser email password uuid = UserSignedUp
    <$> (checkEmailUnique_ s email  >> checkEmailValid_ email)
    <*> pure uuid

  s `apply` UserSignedUp email uuid_ =
    let emails = _emails s
    in s { _emails = Set.insert email emails }

  new aid = System aid (Set.singleton "tuyendv@hpu.edu.vn")

-------------------------- validation ---------------------------------

checkEmailUnique_ s = validate (checkEmailUnique s) EmailMustBeUnique

checkEmailValid_ :: Email -> Either (Error System) Email
checkEmailValid_ email =
  if EV.isValid . encodeUtf8 $ email
  then Right email
  else Left EmailMustBeValid

checkEmailUnique :: System -> Email -> Bool
checkEmailUnique s e = not $ Set.member e (_emails s)

-------------------------- Testing -----------


systemCommand_ = SignUpUser "tuyendv@hpu.edu.vn" "fsdlfjsdlfkj" "user-1"

systemId_ = SystemId "system1"

sampleSystem_ :: System
sampleSystem_ = new systemId_

test :: IO ()
test = print $ sampleSystem_ `handle` systemCommand_


