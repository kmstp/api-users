module Common.Views.Users
where

import Common.Serialization (UserSerialized, age, pet, userKey, userName)
import Control.Lens
import qualified Data.Text as T
import GHC.Show (Show)
import qualified Miso
import Miso.Html
import qualified Miso.String as Miso
import Protolude

renderUsers :: [UserSerialized] -> View action
renderUsers  = renderTable_ ["User", "Name", "Age", "Pet"]

singleHeader_ :: T.Text -> View action
singleHeader_ field = td_ [] [ text $ Miso.ms field ]

singleUser_ :: UserSerialized -> View action
singleUser_ user =
    tr_
    []
    [ td_
      []
      [ text $ Miso.ms (user ^. userKey)]
    , td_
      []
      [ text $ Miso.ms (user ^. userName)]
    , td_
      []
      [ text $ Miso.ms (show (user ^. age) :: T.Text) ]
    , td_
      []
      [ text $ showMaybe $ user ^. pet ]
    ]
    where
      showMaybe = maybe "" Miso.ms


renderHeaders headers =
  tr_
  []
  (fmap singleHeader_ headers)

renderTable_ :: [T.Text] -> [UserSerialized] -> View action
renderTable_ headers users =
  table_
  []
  (renderHeaders headers : fmap singleUser_ users)
