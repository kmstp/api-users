{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Common.RestAPI
where

import Common.Serialization
import Protolude
import Servant.API ((:>), Get, JSON)

type RestAPI = "users" :> Get '[JSON] [UserSerialized]
