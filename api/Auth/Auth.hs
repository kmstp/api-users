{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module Auth.Auth
where

import qualified Data.ByteString as B
import qualified Data.Map as M
import qualified Data.Text as T
import Protolude
import qualified Servant as S
import qualified Servant.Server.Experimental.Auth as EA
-- performing authentication
newtype Account = Account { unAccount :: T.Text } deriving (Show)
type instance EA.AuthServerData (S.AuthProtect "cookie-auth") = Account

-- | A (pure) database mapping keys to accounts.
database :: M.Map B.ByteString Account
database = M.fromList [ ("key1", Account "Anne Briggs")
                    , ("key2", Account "Bruce Cockburn")
                    , ("key3", Account "Ghédalia Tazartès")
                    ]

-- | A method that, when given a password, will return a Account.
-- This is our bespoke (and bad) authentication logic.
lookupAccount :: B.ByteString -> S.Handler Account
lookupAccount key = case M.lookup key database of
  Nothing -> throwError (S.err403 { S.errBody = "Invalid Cookie" })
  Just usr -> return usr
