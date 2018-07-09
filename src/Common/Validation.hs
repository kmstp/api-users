{-# LANGUAGE OverloadedStrings #-}
module Common.Validation
where
import Data.Either.Validation
import Protolude

data Validate a = Validated a | Unvalidated a
  deriving (Show)

notNull  :: Text -> Text -> Validation [Text] Text
notNull name value = if value == ""
                     then Failure [name <> ": not null"]
                     else Success value

{-
len :: Int -> Text -> Text -> Validation [Text] Text
len size name value = if length value /= size then Failure [printf "%s: length must be %d" name size] else Success value
-}
