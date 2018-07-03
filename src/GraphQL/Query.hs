{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
module GraphQL.Query
( RootQueryType
, rootQuery)
where
import Data.Semigroup ((<>))
import Data.Text
import GraphQL
import GraphQL.API ((:>), Argument, Field, Object, Union)
import GraphQL.Resolver ((:<>)(..), Handler, unionValue)

{-
  type Query {
    me: User!
  }

  type User {
    name: Text!
  }

  type Hello {
    greeting(who: String!): String!
  }

  mutation {
    createToken(username: String!, password: String!) {
      token
      error
    }
  }
-}
type HelloType = Object "HelloType" '[]
  '[ Argument "who" Text :> Field "greeting" Text
   ]
type UserType = Object "UserType" '[] '[Field "name" Text]
type RootQueryType = Object "RootQueryType" '[]
  '[
    Field "me" UserType
  , Field "hello" HelloType
  ]
rootQuery :: Applicative m => Text -> Handler m RootQueryType
rootQuery ctx =  pure (me ctx :<> hello)


me :: Applicative m => Text -> Handler m UserType
me ctx = pure name
  where name =
          pure ("Dung" <> ctx)

hello :: Applicative m => Handler m HelloType
hello = pure greeting
  where
    greeting who = pure ("Hello " <> who <> "!")
