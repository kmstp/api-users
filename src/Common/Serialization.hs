{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
module Common.Serialization
where

import Data.Aeson
import Data.Semigroup (mconcat, (<>))
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
      userKey = mconcat [show . DM.userId $ u, "-", toLower $ DM.userName u]
    , userName = DM.userName u
    , age = DM.age u
    , pet = DM.pet u
    }

instance ToJSON UserSerialized
instance FromJSON UserSerialized

