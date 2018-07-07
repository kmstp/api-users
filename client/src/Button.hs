{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Common.Button
  ( -- Note: No constructors are exported
    Model
  , Action

  , initialModel
  , PublicActions(..)
  , updateModel
  , viewModel
  )
  where

import Control.Lens (makeLenses, use, (+=), (.=), (^.))

import Control.Monad (when)
import Data.Monoid ((<>))
import Jwt
import qualified Miso
import Miso.Html
import qualified Miso.String as Miso

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

-- Note the polymorphism in `action`
-- This `action` will be filled in to become the parent's `Action`
-- Also note that this is the Transition monad, rather than the Effect monad
-- See the documentation for the Transition monad in miso's Haddock.
updateModel
    :: PublicActions action
    -> Action
    -> Miso.Transition action Model ()
updateModel pa action = case action of
    MouseDown -> do
      mDownState .= True
      mEnterCount += 1

      enterCount <- use mEnterCount

      when (enterCount == 10) $
        Miso.scheduleIO $ do
          setJwtToken "test"
          pure $ manyClicks pa enterCount


    MouseUp ->
      mDownState .= False

