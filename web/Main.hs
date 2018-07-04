{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Main where

import API
import Protolude

main :: IO ()
main = runServer
