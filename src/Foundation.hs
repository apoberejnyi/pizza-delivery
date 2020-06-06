{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BangPatterns #-}

module Foundation where

import           Auth.Token                    as Token
import qualified Client.OpenCage               as OpenCage
import           Control.Concurrency
import qualified Control.Concurrent.Async      as Async
import           Control.Exception
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Control.Applicative
import           Crypto.Random.Types
import           Crypto.Random.Entropy
import           Data.Address
import qualified Data.UUID.V4                  as UUID
import           Data.Generate.UUID
import           Data.Generate.POSIXTime
import           Data.Generate.UTCTime
import qualified Feature.Order.Gateway.Endpoints
import qualified Feature.Order.Persistence.Repository
import qualified Feature.Order.Persistence.Types
import qualified Feature.Order.Contract
import qualified Feature.Order.Service
import qualified Feature.OrderOption.Gateway.Endpoints
import qualified Feature.OrderOption.Persistence.Repository
import qualified Feature.OrderOption.Persistence.Types
import qualified Feature.OrderOption.Contract
import qualified Feature.OrderOption.Service
import qualified Feature.Restaurant.Gateway.Endpoints
import qualified Feature.Restaurant.Persistence.Repository
import qualified Feature.Restaurant.Persistence.Types
import qualified Feature.Restaurant.Service
import qualified Feature.Restaurant.Contract
import qualified Feature.User.Gateway.Endpoints
import qualified Feature.User.Persistence.Repository
import qualified Feature.User.Persistence.Contract
import qualified Feature.User.Service
import qualified Feature.User.Contract
import           Gateway.Util
import           Network.HTTP.Req
import           System.Envy
import           Data.Time.Clock.POSIX
import           Web.Scotty.Trans
import           GHC.Base

startGateway :: IO ()
startGateway = do
  run <- liftIO runApp
  scottyT 3000 run $ do
    Feature.Order.Gateway.Endpoints.endpoints
    Feature.Restaurant.Gateway.Endpoints.endpoints
    Feature.OrderOption.Gateway.Endpoints.endpoints
    Feature.User.Gateway.Endpoints.endpoints
    notFoundRoute

newtype AppT a = AppT
  { unAppT :: ReaderT JwtConfig IO a
  } deriving  (Applicative, Functor, Monad, MonadIO)

runApp :: IO (AppT a -> IO a)
runApp = do
  !(config :: JwtConfig) <- either error id <$> decodeEnv
  pure $ \app -> (runReaderT . unAppT) app config

instance UUIDGen AppT where
  nextUUID = liftIO UUID.nextRandom

instance Token.Service AppT where
  generate user time = AppT (generateToken user time)
  validate = AppT . validateToken

instance Concurrent AppT where
  concurrently (AppT a) (AppT b) = AppT $ liftIO =<< liftA2
    Async.concurrently
    (returnIO <$> a)
    (returnIO <$> b)

  concurrently3 a b c = do
    (a', (b', c')) <- concurrently a (concurrently b c)
    pure (a', b', c')

instance MonadHttp AppT where
  handleHttpException = throw

instance AddressResolver AppT where
  resolveAddress address = do
    envVars <- either error id <$> liftIO decodeEnv
    OpenCage.resolveAddress envVars address

instance Feature.Order.Contract.Service AppT where
  getAll  = Feature.Order.Service.getAllOrders
  getById = Feature.Order.Service.getOrderById
  place   = Feature.Order.Service.placeOrder
  delete  = Feature.Order.Service.deleteOrder

instance Feature.Order.Persistence.Types.Repo AppT where
  queryAll  = Feature.Order.Persistence.Repository.queryAllOrders
  queryById = Feature.Order.Persistence.Repository.queryOrderById
  insert    = Feature.Order.Persistence.Repository.insertOrder
  delete    = Feature.Order.Persistence.Repository.deleteOrder

instance Feature.OrderOption.Contract.Service AppT where
  getAll         = Feature.OrderOption.Service.getAllOrderOptions
  getById        = Feature.OrderOption.Service.getOrderOptionById
  checkExistence = Feature.OrderOption.Service.checkOrderOptionsExistence
  register       = Feature.OrderOption.Service.registerOrderOption
  delete         = Feature.OrderOption.Service.deleteOrderOption

instance Feature.OrderOption.Persistence.Types.Repo AppT where
  queryAll  = Feature.OrderOption.Persistence.Repository.queryAllOrderOptions
  queryById = Feature.OrderOption.Persistence.Repository.queryOrderOptionById
  filterExisting =
    Feature.OrderOption.Persistence.Repository.filterExistingOrderOptionIds
  insert = Feature.OrderOption.Persistence.Repository.insertOrderOption
  delete = Feature.OrderOption.Persistence.Repository.deleteOrderOption

instance Feature.Restaurant.Contract.Service AppT where
  getAll   = Feature.Restaurant.Service.getAllRestaurants
  getById  = Feature.Restaurant.Service.getRestaurantById
  register = Feature.Restaurant.Service.registerRestaurant
  delete   = Feature.Restaurant.Service.deleteRestaurant

instance Feature.Restaurant.Persistence.Types.Repo AppT where
  queryAll  = Feature.Restaurant.Persistence.Repository.queryAllRestaurants
  queryById = Feature.Restaurant.Persistence.Repository.queryRestaurantById
  insert    = Feature.Restaurant.Persistence.Repository.insertRestaurant
  delete    = Feature.Restaurant.Persistence.Repository.deleteRestaurant

instance Feature.User.Contract.Service AppT where
  login    = Feature.User.Service.login
  register = Feature.User.Service.registerUser

instance Feature.User.Persistence.Contract.Repo AppT where
  insert        = Feature.User.Persistence.Repository.insertUser
  lookupPwdHash = Feature.User.Persistence.Repository.lookupUserPwdHash

instance POSIXTimeGen AppT where
  currentTime = liftIO getPOSIXTime

instance UTCTimeGen AppT where
  currentTime = liftIO getCurrentTime

instance MonadRandom AppT where
  getRandomBytes = liftIO . getEntropy
