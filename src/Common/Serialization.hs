{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Common.Serialization
where

import Data.Aeson
import Data.Text
import qualified Database.Models as DM
import Protolude

data UserSerialized = UserSerialized {
    userKey  :: Text
  , userName :: Text
  , age      :: Int
  , pet      :: Maybe Text
  } deriving (Generic, Show)

class Serialize a b | a -> b where
  serialize :: a -> b

instance Serialize (DM.Model DM.User) UserSerialized where
  serialize (DM.Model u) = UserSerialized {
      userKey = (show . DM.userId $ u) <> "-" <> DM.userName u
    , userName = DM.userName u
    , age = DM.age u
    , pet = DM.pet u
    }

instance ToJSON UserSerialized
instance FromJSON UserSerialized

