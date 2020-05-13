module Base.PG where

import Control.Monad.IO.Class
import Data.Pool
import Database.PostgreSQL.Simple

dbPool :: MonadIO m => m (Pool Connection)
dbPool = liftIO $ createPool (connect connectionInfo) close 1 10 10

withConn :: MonadIO m => (Connection -> IO a) -> m a
withConn action = do
  pool <- dbPool
  liftIO $ withResource pool action

connectionInfo :: ConnectInfo
connectionInfo = defaultConnectInfo
    { connectHost = "localhost"
    , connectDatabase = "PizzaDelivery"
    , connectUser = "postgres"
    , connectPassword = "admin"
    }
