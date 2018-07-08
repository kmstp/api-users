{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module ClientAPI
where

import Common.RestAPI (RestAPI)
import Common.Serialization
import Data.Aeson
import qualified Data.HashMap.Strict as Hm
import Data.Proxy
import GHC.Generics
import JavaScript.JSON.Types.Internal
import Protolude
import Servant.API
import Servant.Client.Core
import Servant.Client.Ghcjs

newtype APIClient m = APIClient
  {
    getUsers ::  m [UserSerialized]
  }

apiClient
    :: forall m
     . RunClient m
    => APIClient m
apiClient = APIClient { .. }
  where
    getUsers = Proxy @RestAPI `clientIn` Proxy @m
