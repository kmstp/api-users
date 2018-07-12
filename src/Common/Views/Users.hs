module Common.Views.Users
where

import Common.Serialization
    (UserSerialized, age, fields, pet, userKey, userName)
import Control.Lens
import Data.Monoid ((<>))
import qualified Data.Text as T
import GHC.Show (Show)
import qualified Miso
import Miso.Html
import qualified Miso.String as Miso
import qualified Miso.String as S
import Protolude

renderUsers :: [UserSerialized] -> View action
renderUsers  = fn2 ["User", "Name", "Age", "Pet"]

fn1 = h2_
  []
  [ text "HTML Table" ]

singleHeader_ :: T.Text -> View action
singleHeader_ field = th_ [] [ text $ Miso.ms field ]

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
      [ text $ S.ms (show (user ^. age) :: T.Text) ]
    , td_
      []
      [ text $ showMaybe $ user ^. pet ]
    ]
    where
      showMaybe = maybe "" S.ms

fn2 :: [T.Text] -> [UserSerialized] -> View action
fn2 headers users = table_
  []
  (fmap singleUser_ users)
