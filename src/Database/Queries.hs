module Database.Queries
where

import Database.Models
import Database.Selda
import Database.Selda.Generic
import Protolude hiding ((:*:))

genericGrownups :: Query s (Col s Text)
genericGrownups = do
  p <- select (gen users)
  restrict (p!sel_age .> 20)
  return $ p!sel_name

getPeopleOfAge :: Int -> SeldaM [User]
getPeopleOfAge yrs = do
  us <- query $ do
    u <- select (gen users)
    restrict (u!sel_age .== literal yrs)
    return u
  return (fromRels us)

getPeopleUnderAge :: Int -> SeldaM [Model User]
getPeopleUnderAge yrs = do
  us <- query $ do
    u <- select (gen users)
    restrict (u!sel_age .<= literal yrs)
    return u
  return $ Model <$> fromRels us
