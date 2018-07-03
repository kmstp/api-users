{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import qualified Common
import Control.Lens (makeLenses, (+=), (-=), (.=), (^.))
import Data.Proxy (Proxy(..))
import Miso (App(..), View)
import qualified Miso
import qualified Miso.String as Miso
import Servant.API ((:<|>)(..))
import qualified Servant.API as Servant
import qualified Servant.Utils.Links as Servant
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
    :: Common.Action
    -> Miso.Transition Common.Action Common.Model ()
updateModel action =
    case action of
        Common.NoOp          -> pure ()
        Common.AddOne        -> Common.counterValue += 1
        Common.SubtractOne   -> Common.counterValue -= 1
        Common.ChangeURI uri ->
          Miso.scheduleIO $ do
            Miso.pushURI uri
            pure Common.NoOp
        Common.HandleURIChange uri -> Common.uri .= uri
