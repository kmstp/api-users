module Database.Queries
where

import Database.Models
import Database.Selda
import Database.Selda.Generic
import Protolude hiding ((:*:))

genericGrownups :: Query s (Col s Text)
genericGrownups = do
  p <- select (gen people)
  restrict (p!sel_age .> 20)
  return $ p!sel_name
