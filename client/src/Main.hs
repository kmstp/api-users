{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
module Main where

import ClientAPI
import qualified Common
import Common.AesonDecode
import qualified Common.Button as B
import Control.Lens (makeLenses, (+=), (-=), (.=), (^.))
import Data.Proxy (Proxy(..))
import qualified Jwt
import Miso (App(..), View)
import qualified Miso
import qualified Miso.String as Miso
import Protolude
import Servant.API ((:<|>)(..))
import qualified Servant.API as Servant
import Servant.Client.Core
import Servant.Client.Ghcjs
import qualified Servant.Links as Servant
import System.IO (IO)

main :: IO ()
main =
  Miso.miso $ \currentURI -> App
    { initialAction = Common.NoOp
    , model         = Common.initialModel currentURI
    , update        = Miso.fromTransition . updateModel
    , view          = Common.viewModel
    , events        = Miso.defaultEvents
    , subs          = [ Miso.uriSub Common.HandleURIChange ]
    , mountPoint    = Nothing
    }

updateModel
    :: Common.Action -> Miso.Transition Common.Action Common.Model ()
updateModel = \case
  Common.NoOp             -> pure ()
  Common.UpdateUsers us   -> Common.users .= us
  Common.AddOne           -> Common.counterValue += 1
  Common.SubtractOne      -> Common.counterValue -= 1
  Common.ChangeURI uri    ->
    Miso.scheduleIO $ do
      Miso.pushURI uri
      ePos <- runClientM $ getUsers apiClient
      case ePos of
        Right users -> pure $ Common.UpdateUsers users
        Left _      -> pure Common.NoOp
  Common.HandleURIChange uri -> Common.uri .= uri


