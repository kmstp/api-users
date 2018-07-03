{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Repository where

import Aggregate as A
import Control.Concurrent.Async (wait)
import Control.Exception.Safe
import Control.Monad
import Control.Monad.Reader
import Data.Aeson
import Data.ByteString.Lazy (fromStrict)
import Data.Data
import Data.Maybe
import Data.Semigroup ((<>))
import Data.Text
import qualified Database.EventStore as ES

type Connection = ES.Connection
type Credentials = ES.Credentials
type WriteResult = ES.WriteResult


getConnection :: IO Connection
getConnection = do
    print "trying connect"
    ES.connect ES.defaultSettings (ES.Static "localhost" 1113)

defaultCredentials = Just $ ES.credentials "admin" "changeit"

readAllEvents :: (Aggregate a, FromJSON (A.Event a), AggregateId (A.Id a)) =>
  Connection
  -> Maybe Credentials
  -> A.Id a
  -> IO a
readAllEvents conn maybeCreds aid  =
  readPortion (new aid)
  where
    stream = textAggregateId aid
    streamName = ES.StreamName stream
    readPortion a = do
      res <- ES.readStreamEventsForward conn streamName 0 500 False maybeCreds >>= wait
      case res of
        ES.ReadSuccess sl -> do
          print sl
          let isEOS = ES.sliceEOS sl
          case applyEvents a sl of
            Just a' | isEOS -> return a'
            Just a'         -> readPortion a'
            Nothing         -> return a
        e -> return a
    applyEvents a s =
      foldM decodeEvent a (ES.sliceEvents s)

    decodeEvent a e =
      apply a <$> (pure (ES.resolvedEventOriginal e) >>= decode . fromStrict . ES.recordedEventData)

writeAllEvents :: (Data (A.Event a), ToJSON (A.Event a), AggregateId (A.Id a)) =>
  Connection
  -> A.Id a
  -> [A.Event a]
  -> Maybe Credentials -> IO WriteResult
writeAllEvents conn aid es creds =
  let packEvent e = ES.createEvent (ES.UserDefined $ pack . show $ toConstr e) Nothing (ES.withJson $ toJSON e)
      stream = ES.StreamName $ textAggregateId aid
  in ES.sendEvents conn stream ES.anyVersion (packEvent <$> es) creds >>= wait

data ActionInput = ActionInput {
  creds :: Maybe Credentials
, conn  :: Connection
}

type ActionM a = ReaderT ActionInput IO a
loadAggregate :: (Aggregate a
                , FromJSON (Event a)
                , AggregateId (Id a))
                => Id a -> ActionM a
loadAggregate aid = do
  conn_ <- reader conn
  creds_ <- reader creds
  x :: a <- liftIO $ readAllEvents conn_ creds_ aid
  pure x


saveAggregate :: (Aggregate a
                , ToJSON (Event a)
                , AggregateId (Id a)
                , Data (Event a))
                => a -> Event a -> ActionM ()
saveAggregate u ev = do
  conn_ <- reader conn
  creds_ <- reader creds
  _ <- liftIO $ writeAllEvents conn_ (aggregateId u) [ev] creds_
  return ()
