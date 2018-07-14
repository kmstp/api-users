{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Common.GraphQLAPI
where

import qualified Data.Text as T
import qualified GraphQL as G
import qualified GraphQL.Request as GR
import Servant.API

type GraphQLAPI =
     Header "Bearer" T.Text
  :> "graphql"
  :> ReqBody '[JSON] GR.Request
  :> Post '[JSON] G.Response
