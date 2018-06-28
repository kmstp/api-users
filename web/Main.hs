{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module Main where

import           API

main :: IO ()
main = runServer

test = print "Hello 1"
