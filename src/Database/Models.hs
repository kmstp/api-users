{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
module Database.Models
where

import Database.Selda
import Database.Selda.Generic
import Protolude hiding ((:*:))

data Person = Person
  { personId   :: RowID
  , personName :: Text
  , age        :: Int
  , pet        :: Maybe Text
  } deriving Generic

data Address = Address
  { addrId   :: RowID
  , addrName :: Text
  , city     :: Text
  } deriving Generic

sel_personid :*: sel_name :*: sel_age :*: sel_pet = selectors (gen people)

people :: GenTable Person
people = genTable "people" [personId :- autoPrimaryGen]

addresses :: GenTable Address
addresses = genTable "addresses" [addrId :- autoPrimaryGen]
