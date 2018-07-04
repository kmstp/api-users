{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
module Lib
    ( someFunc
    ) where

import Protolude

someFunc :: IO ()
someFunc =
  print "hello"

