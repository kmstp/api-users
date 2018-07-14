{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Common where

import Control.Lens hiding ((.=))
import Data.Proxy (Proxy(..))
import Servant.API ((:<|>)(..), (:>), JSON)
import qualified Servant.API as Servant
#if MIN_VERSION_servant(0,10,0)
import qualified Servant.Utils.Links as Servant
#endif
import qualified Common.Button as B
import qualified Common.Serialization as CS
import Common.Views.Users
import Data.Aeson
import qualified Data.Text as T
import Miso (View)
import qualified Miso
import Miso.Html
import qualified Miso.String as Miso
import qualified Network.URI as Network
import Network.URI.JSON
import Protolude

data HomeModel = HomeModel { _users :: [CS.UserSerialized], _headers :: [T.Text] }
  deriving (Eq, Generic, Show, FromJSON, ToJSON)

initialHomeModel = HomeModel { _users = [], _headers = ["User", "Name", "Age", "Pet"] }

makeLenses ''HomeModel

type UserList = [CS.UserSerialized]

newtype SubModel = Home HomeModel
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

makePrisms ''SubModel

data Model
   = Model
     { _uri          :: !Network.URI
     , _counterValue :: !Int
     , _name         :: !T.Text
     , _model        :: !SubModel
     }
    deriving (Eq, Generic, Show, FromJSON, ToJSON)

makeLenses ''Model

initialModel :: Network.URI -> Model
initialModel uri =
    Model
    { _uri = uri
    , _counterValue = 0
    , _name = "Dung"
    , _model = Home initialHomeModel
    }

data Action
  = NoOp
  | AddOne
  | SubtractOne
  | ChangeURI !Network.URI
  | HandleURIChange !Network.URI
  | FetchMe
  | UpdateModel !SubModel
  deriving (Show, Eq)

-- Holds a servant route tree of `View action`
type ViewRoutes = Home :<|> Flipped

-- Home route, contains two buttons and a field
type Home = View Action

-- Flipped route, same as Home, but with the buttons flipped
type Flipped = "flipped" :> View Action


-- Checks which URI is open and shows the appropriate view
viewModel :: Model -> View Action
viewModel m =
    case Miso.runRoute (Proxy @ViewRoutes) viewTree _uri m of
      Left _routingError -> page404View
      Right v -> v

-- Servant tree of view functions
-- Should follow the structure of ViewRoutes
viewTree
    ::      (Model -> View Action)
       :<|> (Model -> View Action)
viewTree = homeView :<|> flippedView

headerLayout_ :: View Action
headerLayout_ = header_
  []
  [ h1_
    []
    [ text "This is my branding" ]
  , p_
    []
    [ text "Say something about website" ]
  ]

navMenu_ = nav_
  []
  [ ul_
    []
    [ li_
      []
      [ a_
        [ onClick $ ChangeURI homeLink ] [ text "Home" ]
      ]
    , li_
      []
      [ a_
        [ onClick $ ChangeURI flippedLink ] [ text "Flipped" ]
      ]
    ]
  ]

footer__ = footer_
  []
  [ p_
    []
    [ text "HyHy \169 2018" ]
  ]

withLayout_ content_ =
  div_
  []
  [
    headerLayout_
    ,
    navMenu_
    ,
    content_
    ,
    footer__
  ]
-- View function of the Home route
homeView :: Model -> View Action
homeView m = withLayout_ $
      div_ []
      [ div_
          []
          [ button_ [ onClick SubtractOne ] [ text "-" ]
          , text $ Miso.ms (show (_counterValue m) :: T.Text)
          , button_ [ onClick AddOne ] [ text "+" ]
          ]
      --, button_ [ onClick $ ChangeURI flippedLink ] [ text "Go to /flipped" ]
      , renderUsers $ m ^. model ._Home . users
      ]

-- View function of the Home route
flippedView :: Model -> View Action
flippedView m = withLayout_ $
    div_ []
      [ div_
        []
        [ button_ [ onClick AddOne ] [ text "+" ]
        , text $ Miso.ms (show  (_counterValue m) :: T.Text)
        , button_ [ onClick SubtractOne ] [ text "-" ]
        ]
      , button_ [ onClick $ ChangeURI homeLink ] [ text "Go to /" ]
      ]

page404View :: View Action
page404View =
    text "Yo, 404, page unknown. Go to / or /flipped. Shoo!"

-- Network.URI that points to the home route
homeLink :: Network.URI
homeLink =
#if MIN_VERSION_servant(0,10,0)
    Servant.linkURI $ Servant.safeLink (Proxy @ViewRoutes) (Proxy @Home)
#else
    Servant.safeLink (Proxy @ViewRoutes) (Proxy @Home)
#endif

-- Network.URI that points to the flipped route
flippedLink :: Network.URI
flippedLink =
#if MIN_VERSION_servant(0,10,0)
    Servant.linkURI $ Servant.safeLink (Proxy @ViewRoutes) (Proxy @Flipped)
#else
  Servant.safeLink (Proxy @ViewRoutes) (Proxy @Flipped)
#endif
