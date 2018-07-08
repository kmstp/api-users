module Common.Params
where

import Data.Aeson
import Protolude

data UserParams = UserParams {
    userName :: Text
  , age      :: Int
  , pet      :: Maybe Text
} deriving (Show, Generic)

instance FromJSON UserParams
