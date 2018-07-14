{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
module Main where

import ClientAPI
import qualified Common
import Common.AesonDecode
import qualified Common.Button as B
import Common.IsoAPI (ClientAPI)
import Control.Lens (makeLenses, (+=), (-=), (.=), (^.))
import Data.Proxy
import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Jwt
import Miso (App(..), View)
import qualified Miso
import qualified Miso.String as Miso
import Network.URI
import qualified Prelude as P
import Protolude
import Servant.API ((:<|>)(..))
import qualified Servant.API as Servant
import Servant.Client.Core
import Servant.Client.Ghcjs
import qualified Servant.Links as Servant
import Servant.Match
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
  Common.UpdateModel model   -> Common.model .= model
  Common.AddOne           -> Common.counterValue += 1
  Common.SubtractOne      -> Common.counterValue -= 1
  Common.ChangeURI uri    ->
    Miso.scheduleIO $ do
      Miso.pushURI uri
      eval uri
  Common.HandleURIChange uri ->
    Common.uri .= uri


homeServer = do
  ePos <- runClientM $ getHomeModel apiClient
  case ePos of
    Right model -> pure $ Common.UpdateModel $ model ^. Common.model
    Left _      -> pure Common.NoOp


matchURI' :: Matches api => Proxy api -> MatchT api a -> P.String -> Maybe a
matchURI' proxy parser s = case
  parseURI s of
    Just u -> matchURI proxy parser u
    Nothing -> Nothing

data ClientView
  = HomeView
  deriving (Show, Eq)

parser :: MatchT ClientAPI ClientView
parser = HomeView

match = matchURI' (Proxy :: Proxy ClientAPI) parser

eval uri = case match ("http://localhost:3003/" <> show uri) of
  Just HomeView -> homeServer
  Nothing -> do
    print uri
    print "aaaa"
    pure Common.NoOp
