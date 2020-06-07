{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RecordWildCards #-}

module Persistence.PG
  ( withConn
  , initPool
  , PG
  )
where


import           Control.Monad.Reader
import           Data.Pool
import           Data.Has
import           Database.PostgreSQL.Simple
import           System.Envy

type PG r m = (MonadReader r m, Has (Pool Connection) r, MonadIO m)

withConn :: PG r m => (Connection -> IO a) -> m a
withConn action = do
  pool <- asks getter
  liftIO $ withResource pool action

initPool :: MonadIO m => m (Pool Connection)
initPool = do
  ci <- liftIO decodeEnv
  case ci of
    Left  message        -> error message
    Right connectionInfo -> do
      let connection = connect $ populateConnectionDefaults connectionInfo
      liftIO $ createPool connection close 1 10 10

data PGConnectInfo = PGConnectInfo
    { pgHost     :: String
    , pgUser     :: String
    , pgPassword :: String
    , pgDatabase :: String
    }

instance FromEnv PGConnectInfo where
  fromEnv _ =
    PGConnectInfo
      <$> env "POSTGRES_HOST"
      <*> env "POSTGRES_USER"
      <*> env "POSTGRES_PASSWORD"
      <*> env "POSTGRES_DB"

populateConnectionDefaults :: PGConnectInfo -> ConnectInfo
populateConnectionDefaults PGConnectInfo {..} = defaultConnectInfo
  { connectHost     = pgHost
  , connectDatabase = pgDatabase
  , connectUser     = pgUser
  , connectPassword = pgPassword
  }
