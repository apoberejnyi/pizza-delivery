{-# LANGUAGE RecordWildCards #-}

module Persistence.PG
    ( withConn
    , checkPGEnv
    ) where

import Control.Monad.IO.Class
import Data.Pool
import Database.PostgreSQL.Simple
import System.Envy

checkPGEnv :: IO ()
checkPGEnv = () <$ connectionPool

connectionPool :: MonadIO m => m (Pool Connection)
connectionPool = do
    ci <- liftIO decodeEnv
    case ci of
        Left message -> error message
        Right connectionInfo -> do
            let connection = connect $ populateConnectionDefaults connectionInfo
            liftIO $ createPool connection close 1 10 10

withConn :: MonadIO m => (Connection -> IO a) -> m a
withConn action = do
  pool <- connectionPool
  liftIO $ withResource pool action

data PGConnectInfo = PGConnectInfo
    { pgHost     :: String
    , pgUser     :: String
    , pgPassword :: String
    , pgDatabase :: String
    }

instance FromEnv PGConnectInfo where
  fromEnv _ = PGConnectInfo
    <$> env "PG_HOST"
    <*> env "PG_USER"
    <*> env "PG_PASSWORD"
    <*> env "PG_DATABASE"

populateConnectionDefaults :: PGConnectInfo -> ConnectInfo
populateConnectionDefaults PGConnectInfo{..} = defaultConnectInfo
    { connectHost = pgHost
    , connectDatabase = pgDatabase
    , connectUser = pgUser
    , connectPassword = pgPassword
    }
