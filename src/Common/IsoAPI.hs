{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Common.IsoAPI
where

import Common
import qualified Miso
import Protolude
import Servant.API ((:<|>), (:>), Get, JSON)

type ClientAPI = Get '[JSON] Common.Model

type IsoAPI = "api" :>  ClientAPI
