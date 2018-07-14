module Common.Actions.Home
where

import Common.Models.Home
import qualified Network.URI as Network
import Protolude

data HomeAction
  = NoOp
  | ChangeURI !Network.URI
  | HandleURIChange !Network.URI
  | UpdateModel HomeModel
  deriving Eq

