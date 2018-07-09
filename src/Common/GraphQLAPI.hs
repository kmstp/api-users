{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Common.GraphQLAPI
where

import qualified GraphQL as G
import qualified GraphQL.Request as GR
import Servant.API

type GraphQLAPI = "graphql"
  :> AuthProtect "cookie-auth"
  :> ReqBody '[JSON] GR.Request
  :> Post '[JSON] G.Response
