{-# LANGUAGE DeriveGeneric #-}

module GraphQL.Request
where

import Data.Aeson (FromJSON, ToJSON)
import Protolude

data Request = Request {
  query :: Text
} deriving (Show, Generic)

instance FromJSON Request
