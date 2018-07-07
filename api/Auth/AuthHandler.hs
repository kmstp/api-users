{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Auth.AuthHandler
where

import qualified Auth.Auth as A
import qualified Data.Map as M
import qualified Network.Wai as W
import Protolude
import qualified Servant as S
import Servant.Server (Context((:.)))
import qualified Servant.Server as SS
import qualified Servant.Server.Experimental.Auth as EA
import qualified Web.Cookie as C

genAuthServerContext :: SS.Context (EA.AuthHandler W.Request A.Account ': '[])
genAuthServerContext = authHandler :. SS.EmptyContext

authHandler :: EA.AuthHandler W.Request A.Account
authHandler = EA.mkAuthHandler handler
  where
  maybeToEither e = maybe (Left e) Right
  throw401 msg = throwError $ S.err401 { S.errBody = msg }
  handler req = either throw401 A.lookupAccount $ do
    cookie <-
      maybeToEither "Missing cookie header" $ M.lookup "cookie" $ M.fromList $ W.requestHeaders req
    maybeToEither "Missing token in cookie" $ M.lookup "servant-auth-cookie" $ M.fromList $ C.parseCookies cookie
