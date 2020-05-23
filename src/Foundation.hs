{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Foundation where

import Base.Concurrency
import Base.HTTP
import Base.Types.Address
import Base.Types.UUID
import qualified Client.OpenCage as OpenCage
import qualified Control.Concurrent.Async as Async
import Control.Exception
import Control.Monad.IO.Class
import qualified Data.UUID.V4 as UUID
import qualified Feature.Order.Contract
import qualified Feature.Order.Gateway.Endpoints
import qualified Feature.Order.Persistence.Contract
import qualified Feature.Order.Persistence.Repository
import qualified Feature.Order.Service
import qualified Feature.OrderOption.Contract
import qualified Feature.OrderOption.Gateway.Endpoints
import qualified Feature.OrderOption.Persistence.Contract
import qualified Feature.OrderOption.Persistence.Repository
import qualified Feature.OrderOption.Service
import qualified Feature.Restaurant.Contract
import qualified Feature.Restaurant.Gateway.Endpoints
import qualified Feature.Restaurant.Persistence.Contract
import qualified Feature.Restaurant.Persistence.Repository
import qualified Feature.Restaurant.Service
import Network.HTTP.Req
import System.Envy
import Web.Scotty.Trans

startGateway :: IO ()
startGateway = scottyT 3000 unAppT $ do
    Feature.Order.Gateway.Endpoints.endpoints
    Feature.Restaurant.Gateway.Endpoints.endpoints
    Feature.OrderOption.Gateway.Endpoints.endpoints
    notFoundRoute

newtype AppT a = AppT
  { unAppT :: IO a
  } deriving  (Applicative, Functor, Monad, MonadIO)

instance UUIDGen AppT where
    nextUUID = liftIO UUID.nextRandom

instance Concurrent AppT where
    concurrently a b = liftIO $ Async.concurrently (unAppT a) (unAppT b)

instance MonadHttp AppT where
    handleHttpException = throw

instance AddressResolver AppT where
    resolveAddress address = do
        envVars <- either error id <$> liftIO decodeEnv
        OpenCage.resolveAddress envVars address

instance Feature.Order.Contract.Service AppT where
    getAll = Feature.Order.Service.getAllOrders
    placeOrder = Feature.Order.Service.placeOrder

instance Feature.Order.Persistence.Contract.Repo AppT where
    queryAll = Feature.Order.Persistence.Repository.queryAllOrders
    insert = Feature.Order.Persistence.Repository.insertOrder

instance Feature.OrderOption.Contract.Service AppT where
    getAll = Feature.OrderOption.Service.getAllOrderOptions
    getById = Feature.OrderOption.Service.getOrderOptionById
    register = Feature.OrderOption.Service.registerOrderOption
    delete = Feature.OrderOption.Service.deleteOrderOption

instance Feature.OrderOption.Persistence.Contract.Repo AppT where
    queryAll = Feature.OrderOption.Persistence.Repository.queryAllOrderOptions
    queryById = Feature.OrderOption.Persistence.Repository.queryOrderOptionById
    insert = Feature.OrderOption.Persistence.Repository.insertOrderOption
    delete = Feature.OrderOption.Persistence.Repository.deleteOrderOption

instance Feature.Restaurant.Contract.Service AppT where
    getAll = Feature.Restaurant.Service.getAllRestaurants
    getById = Feature.Restaurant.Service.getRestaurantById
    register = Feature.Restaurant.Service.registerRestaurant
    delete = Feature.Restaurant.Service.deleteRestaurant

instance Feature.Restaurant.Persistence.Contract.Repo AppT where
    queryAll = Feature.Restaurant.Persistence.Repository.queryAllRestaurants
    queryById = Feature.Restaurant.Persistence.Repository.queryRestaurantById
    insert = Feature.Restaurant.Persistence.Repository.insertRestaurant
    delete = Feature.Restaurant.Persistence.Repository.deleteRestaurant
