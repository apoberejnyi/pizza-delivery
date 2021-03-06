{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Feature.Order.Gateway.Endpoints where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import qualified Data.Text.Lazy                as LT
import           Feature.Order.Gateway.DTO
import           Feature.Order.Error
import           Feature.Order.Contract        as Order
import           Feature.Order.Types           as Order
import           Feature.Auth.Contract         as Auth
import           Gateway.Auth
import           Gateway.Error
import           Gateway.Util
import           Network.HTTP.Types
import           Web.Scotty.Trans              as S

type OrderHttpMonad m = (MonadIO m, Order.Service m, Auth.Service m)

endpoints :: OrderHttpMonad m => ScottyT LT.Text m ()
endpoints = do
  get "/api/orders" $ do
    result <- lift Order.getAll
    json (fmap toDTO result :: [OrderDto])

  get "/api/order/:id" $ do
    oid    <- uuidParam "orderId"
    result <- lift $ Order.getById (OrderId oid)
    case result of
      Left  err@(OrderNotFound _) -> httpError notFound404 err
      Right order                 -> json (toDTO order :: OrderDto)

  post "/api/orders" $ do
    userId' <- requestUserId
    (payloadDto :: IffyOrderPayloadDto) <- parseBody
    result <- lift $ Order.place userId' (fromDTO payloadDto)
    case result of
      Right order -> S.status created201 >> json (toDTO order :: OrderDto)
      Left  err@NoRestaurantsAvailable -> httpError notFound404 err
      Left  err@AddressNotFound        -> httpError notFound404 err
      Left  err@(AmbiguousAddress   _) -> httpError badRequest400 err
      Left  err@(UnknownOrderOption _) -> httpError notFound404 err

  S.delete "/api/orders/:id" $ do
    oid    <- uuidParam "orderId"
    result <- lift $ Order.delete (OrderId oid)
    case result of
      Left  err@(OrderDidNotExist _) -> httpError notFound404 err
      Right _                        -> finish
