{-# LANGUAGE FlexibleContexts #-}
module Play
where

import Control.Monad.IO.Class
import Control.Monad.State
import Control.Monad.Writer

--introduce :: (MonadIO m, MonadWriter [String] m)
--          => String -> String -> m ()
introduce name1 name2 = do
  st <- get
  tell "Dung"
  liftIO $ putStrLn (name1 ++ ", this is " ++ name2)
  x <- liftIO  getLine
  tell "hehe"
  put (st * (read x :: Int))
  liftIO $ putStrLn (name2 ++ ", this is " ++ name1)


test = do
  print "Hello"
  x <- runWriterT (runStateT (introduce "test" "test2") 10)
  print x
