{-# LANGUAGE DeriveGeneric #-}

module GraphQL.Request
where

import Data.Aeson (FromJSON, Object, ToJSON)
import qualified Data.Text.Lazy as TL
import Protolude

data Request = Request {
  query     :: Text
, variables :: TL.Text
} deriving (Show, Generic)

instance FromJSON Request
