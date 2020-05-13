{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Foundation where

import Base.UUID
import Control.Monad.IO.Class
import qualified Data.UUID.V4 as UUID
import Database.PostgreSQL.Simple
import qualified Feature.OrderOption.Contract as OrderOption
import qualified Feature.OrderOption.Gateway.Endpoints as OrderOptionGateway
import Feature.OrderOption.Persistence
import Feature.OrderOption.Service
import Web.Scotty.Trans

startGateway :: IO ()
startGateway = scottyT 3000 unAppT OrderOptionGateway.endpoints

newtype AppT a = AppT
  { unAppT :: IO a
  } deriving  (Applicative, Functor, Monad, MonadIO)

instance UUIDGen AppT where
    nextUUID = liftIO UUID.nextRandom

instance OrderOption.Service AppT where
    getAll = liftIO $ mkPostgreSQLConnection >>= queryAllOrderOptions
    register = registerOrderOption

instance OrderOption.Repo AppT where
    queryAll = liftIO $ mkPostgreSQLConnection >>= queryAllOrderOptions
    insert oo = do
        conn <- liftIO mkPostgreSQLConnection
        liftIO $ insertOrderOption conn oo

mkPostgreSQLConnection :: IO Connection
mkPostgreSQLConnection = connect defaultConnectInfo
    { connectHost = "localhost"
    , connectDatabase = "PizzaDelivery"
    , connectUser = "postgres"
    , connectPassword = "admin"
    }
