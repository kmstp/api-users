{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
module ClientAPI
where

import Common
import Common.IsoAPI (IsoAPI)
import Common.Serialization
import Control.Lens
import Data.Aeson
import Data.Either
import qualified Data.HashMap.Strict as Hm
import Data.Proxy
import GHC.Generics
import Miso
import Network.URI
import Protolude
import Servant.API
import Servant.Client.Core
import Servant.Client.Ghcjs

newtype APIClient m = APIClient
  {
    getHomeModel ::  m Common.Model
  }

apiClient
    :: forall m
     . RunClient m
    => APIClient m
apiClient = APIClient { .. }
  where
    getHomeModel = Proxy @IsoAPI `clientIn` Proxy @m

