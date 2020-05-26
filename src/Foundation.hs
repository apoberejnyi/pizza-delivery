{-# LANGUAGE DeriveFunctor #-}
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
import qualified Feature.Order.Gateway.Endpoints
import qualified Feature.Order.Persistence.Repository
import qualified Feature.Order.Persistence.Types
import qualified Feature.Order.Service
import qualified Feature.Order.Types
import qualified Feature.OrderOption.Gateway.Endpoints
import qualified Feature.OrderOption.Persistence.Repository
import qualified Feature.OrderOption.Persistence.Types
import qualified Feature.OrderOption.Service
import qualified Feature.OrderOption.Types
import qualified Feature.Restaurant.Gateway.Endpoints
import qualified Feature.Restaurant.Persistence.Repository
import qualified Feature.Restaurant.Persistence.Types
import qualified Feature.Restaurant.Service
import qualified Feature.Restaurant.Types
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
    concurrently3 a b c = do
        (a', (b', c')) <- concurrently a (concurrently b c)
        pure (a', b', c')

instance MonadHttp AppT where
    handleHttpException = throw

instance AddressResolver AppT where
    resolveAddress address = do
        envVars <- either error id <$> liftIO decodeEnv
        OpenCage.resolveAddress envVars address

instance Feature.Order.Types.Service AppT where
    getAll = Feature.Order.Service.getAllOrders
    getById = Feature.Order.Service.getOrderById
    place = Feature.Order.Service.placeOrder
    delete = Feature.Order.Service.deleteOrder

instance Feature.Order.Persistence.Types.Repo AppT where
    queryAll = Feature.Order.Persistence.Repository.queryAllOrders
    queryById = Feature.Order.Persistence.Repository.queryOrderById
    insert = Feature.Order.Persistence.Repository.insertOrder
    delete = Feature.Order.Persistence.Repository.deleteOrder

instance Feature.OrderOption.Types.Service AppT where
    getAll = Feature.OrderOption.Service.getAllOrderOptions
    getById = Feature.OrderOption.Service.getOrderOptionById
    checkExistence = Feature.OrderOption.Service.checkOrderOptionsExistence
    register = Feature.OrderOption.Service.registerOrderOption
    delete = Feature.OrderOption.Service.deleteOrderOption

instance Feature.OrderOption.Persistence.Types.Repo AppT where
    queryAll = Feature.OrderOption.Persistence.Repository.queryAllOrderOptions
    queryById = Feature.OrderOption.Persistence.Repository.queryOrderOptionById
    filterExisting = Feature.OrderOption.Persistence.Repository.filterExistingOrderOptionIds
    insert = Feature.OrderOption.Persistence.Repository.insertOrderOption
    delete = Feature.OrderOption.Persistence.Repository.deleteOrderOption

instance Feature.Restaurant.Types.Service AppT where
    getAll = Feature.Restaurant.Service.getAllRestaurants
    getById = Feature.Restaurant.Service.getRestaurantById
    register = Feature.Restaurant.Service.registerRestaurant
    delete = Feature.Restaurant.Service.deleteRestaurant

instance Feature.Restaurant.Persistence.Types.Repo AppT where
    queryAll = Feature.Restaurant.Persistence.Repository.queryAllRestaurants
    queryById = Feature.Restaurant.Persistence.Repository.queryRestaurantById
    insert = Feature.Restaurant.Persistence.Repository.insertRestaurant
    delete = Feature.Restaurant.Persistence.Repository.deleteRestaurant
