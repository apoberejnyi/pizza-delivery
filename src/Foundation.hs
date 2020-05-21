{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Foundation where

import Base.Types.UUID
import Control.Monad.IO.Class
import qualified Data.UUID.V4 as UUID
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
import Web.Scotty.Trans

startGateway :: IO ()
startGateway = scottyT 3000 unAppT $ do
    Feature.Restaurant.Gateway.Endpoints.endpoints
    Feature.OrderOption.Gateway.Endpoints.endpoints
    -- TODO: add nice default "not found" message

newtype AppT a = AppT
  { unAppT :: IO a
  } deriving  (Applicative, Functor, Monad, MonadIO)

instance UUIDGen AppT where
    nextUUID = liftIO UUID.nextRandom

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
    register = Feature.Restaurant.Service.registerRestaurant

instance Feature.Restaurant.Persistence.Contract.Repo AppT where
    queryAll = Feature.Restaurant.Persistence.Repository.queryAllRestaurants
    insert = Feature.Restaurant.Persistence.Repository.insertRestaurant
