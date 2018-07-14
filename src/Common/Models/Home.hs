{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Common.Models.Home
where

import Common.Serialization (UserSerialized)
import Control.Lens
import Data.Aeson
import Protolude

newtype HomeModel = HomeModel {
    _users :: [UserSerialized]
  } deriving (Eq, Generic)

instance ToJSON HomeModel
instance FromJSON HomeModel

makeLenses ''HomeModel
