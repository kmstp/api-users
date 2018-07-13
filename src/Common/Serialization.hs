{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
module Common.Serialization
where

import Control.Lens
import Data.Aeson
import Data.Data
import Data.Semigroup (mconcat, (<>))
import Data.Text
import qualified Database.Models as DM
import Protolude

data UserSerialized = UserSerialized {
    _userKey  :: Text
  , _userName :: Text
  , _age      :: Int
  , _pet      :: Maybe Text
  } deriving (Generic, Show, Data, Typeable)

makeLenses ''UserSerialized

instance Eq UserSerialized where
  a == b = a ^. userKey == b ^. userKey

class Serialize a b | a -> b where
  serialize :: a -> b

instance Serialize (DM.Model DM.User) UserSerialized where
  serialize (DM.Model u) = UserSerialized {
      _userKey = mconcat [show . DM.userId $ u, "-", toLower $ DM.userName u]
    , _userName = DM.userName u
    , _age = DM.age u
    , _pet = DM.pet u
    }

instance ToJSON UserSerialized
instance FromJSON UserSerialized

-- fields :: forall a . (Data a, Typeable a) => [Text]
-- fields =  foldMap (fmap pack . constrFields) <$> dataTypeConstrs . dataTypeOf $ (undefined :: a)

--getFields :: (Data a, Typeable a) => a -> [Text]
--getFields  = fmap pack . constrFields . toConstr
-- fields = fmap (pack . showConstr) . dataTypeConstrs . dataTypeOf $ (undefined :: @a)
