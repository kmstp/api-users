{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
module Database.Models
where

import Database.Selda
import Database.Selda.Generic
import Protolude hiding ((:*:))

newtype Model a = Model { unModel :: a }
  deriving (Show)

data User = User
  { userId   :: RowID
  , userName :: Text
  , age      :: Int
  , pet      :: Maybe Text
  } deriving (Generic, Show)

data Address = Address
  { addrId   :: RowID
  , addrName :: Text
  , city     :: Text
  } deriving Generic

data Post = Post
  { postId   :: RowID
  , authorId :: RowID
  , postBody :: Text
  } deriving Generic

sel_userid :*: sel_name :*: sel_age :*: sel_pet = selectors (gen users)

users :: GenTable User
users = genTable "users" [userId :- autoPrimaryGen]

addresses :: GenTable Address
addresses = genTable "addresses" [addrId :- autoPrimaryGen]

posts :: GenTable Post
posts = genTable "posts" [postId :- autoPrimaryGen, authorId :- fkGen (gen users) sel_userid]


person1 = User def "Test" 35 (Just "ok")
person2 = User def "test2" 36 Nothing
person3 = User def "dung" 25 (Just "good")
sampleUsers = [person1, person2, person3]
