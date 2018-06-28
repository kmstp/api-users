{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
module Database
where

import           Control.Monad.IO.Class      (MonadIO)
import           Control.Monad.Logger        (LoggingT, runStdoutLoggingT)
import           Control.Monad.Reader        (runReaderT)
import           Database.Persist.Postgresql (ConnectionString, SqlPersistT,
                                              runMigration, withPostgresqlConn)

import           Control.Exception           (SomeException)
import           Control.Monad.Catch
import           Data.Int
import           Data.Text
import           Database.Persist            (Entity, SelectOpt (..),
                                              selectList, (<.), (==.))
import           Database.Persist.Sql
import           Schemas

localConnString :: ConnectionString
localConnString = "host=127.0.0.1 port=5432 user=macbookpro dbname=kms password=Dung123#"

fetchPostgresConnection :: IO ConnectionString
fetchPostgresConnection = return localConnString

runAction :: ConnectionString
          -> SqlPersistT (LoggingT IO) a
          -> IO a
runAction connectionString action =
  runStdoutLoggingT
  $ withPostgresqlConn connectionString
  $ \backend -> runReaderT action backend


migrateDB :: ConnectionString -> IO ()
migrateDB connString = runAction connString (runMigration migrateAll)

selectYoungTeachers :: (MonadIO m) => SqlPersistT m [Entity User]
selectYoungTeachers =
  selectList
  [UserAge <. 25, UserOccupation ==. "Teacher"] []

selectYoungTeachers' :: (MonadIO m) => SqlPersistT m [Entity User]
selectYoungTeachers' =
  selectList
 [UserAge <. 25, UserOccupation ==. "Teacher"]
 [Asc UserEmail, OffsetBy 5, LimitTo 100]

sampleUser :: User
sampleUser = User
  { userName = "admin"
  , userEmail = "admin@test.com"
  , userAge = 23
  , userOccupation = "System Administrator"
  }

fetchUserPG :: ConnectionString -> Int64 -> IO (Maybe User)
fetchUserPG connString uid = runAction connString (get (toSqlKey uid))

type DbError = Text

createUserPG :: ConnectionString -> User -> IO (Either DbError Int64)
createUserPG connString user = runAction connString $
  (Right . fromSqlKey <$> insert user) `catch` (\(SomeException _) -> return $ Left "Db Error")

deleteUserPG :: ConnectionString -> Int64 -> IO ()
deleteUserPG connString uid = runAction connString (delete userKey)
  where
    userKey :: Key User
    userKey = toSqlKey uid
