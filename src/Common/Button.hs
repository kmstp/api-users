{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Common.Button
  ( -- Note: No constructors are exported
    Model
  , Action

  , initialModel
  , PublicActions(..)
  , viewModel
  )
  where

import Control.Lens (makeLenses, use, (+=), (.=), (^.))
import Control.Monad (when)
import qualified Miso
import Data.Monoid ((<>))
import Miso.Html
import qualified Miso.String as Miso
import Protolude hiding ((<>), show)
import GHC.Show (Show)
-- Internal state
data Model
   = Model
     { _mDownState  :: !Bool
     , _mText       :: !Miso.MisoString
     , _mEnterCount :: !Int
     }
     deriving (Eq)

-- Some lenses
makeLenses ''Model

-- Demand a button text from above,
-- use defaults for the rest
initialModel :: Miso.MisoString -> Model
initialModel txt =
    Model
    { _mDownState  = False
    , _mText       = txt
    , _mEnterCount = 0
    }

-- Actions interface
-- These actions are interesting for the parent
data PublicActions action
   = PublicActions
     { -- toParent to channel Actions back to this component
       toParent   :: Action -> action

       -- Two events that the parent should do something with
     , click      :: action
     , manyClicks :: Int -> action
     }

data Action
   = MouseDown
   | MouseUp
   deriving (Show, Eq)

-- Same pattern as the `update` function
viewModel :: PublicActions action -> Model -> Miso.View action
viewModel pa m =
    button_
      [ onClick $ click pa
      , onMouseDown $ toParent pa MouseDown
      , onMouseUp $ toParent pa MouseUp
      ]
      [ if m ^. mDownState
        then text $ "~" <> m ^. mText <> "~"
        else text $ m ^. mText
      ]
