{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Foundation where

import Base.Types.UUID
import Control.Monad.IO.Class
import qualified Data.UUID.V4 as UUID
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
    getAll = getAllOrderOptions
    register = registerOrderOption

instance OrderOption.Repo AppT where
    queryAll = queryAllOrderOptions
    insert = insertOrderOption
