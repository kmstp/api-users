module Main where

import           Database

main :: IO ()
main =
  migrateDB localConnString

