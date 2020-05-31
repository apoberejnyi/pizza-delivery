{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Feature.Order.Gateway.Endpoints where

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import qualified Data.Text.Lazy as LT
import Feature.Order.Gateway.DTO
import Feature.Order.Types as Order
import Gateway.Error
import Gateway.Util
import Network.HTTP.Types
import Web.Scotty.Trans as S

endpoints :: (MonadIO m, Order.Service m) => ScottyT LT.Text m ()
endpoints = do
    get "/api/orders" $ do
        result <- lift Order.getAll
        json (fmap toDTO result :: [OrderDto])

    get "/api/order/:id" $ do
        oid <- uuidParam "orderId"
        result <- lift $ Order.getById (OrderId oid)
        case result of
            Left err@(OrderNotFound _) -> httpError notFound404 err
            Right order                -> json (toDTO order :: OrderDto)

    post "/api/orders" $ do
        (payload :: IffyOrderPayloadDto) <- parseBody
        result <- lift $ Order.place (fromDTO payload)
        case result of
            Right order -> status created201 >> json (toDTO order :: OrderDto)
            Left err@NoRestaurantsAvailable -> httpError badRequest400 err
            Left err@AddressNotFound -> httpError badRequest400 err
            Left err@(AmbiguousAddress _) -> httpError badRequest400 err
            Left err@(UnknownOrderOption _) -> httpError badRequest400 err

    S.delete "/api/orders/:id" $ do
        oid <- uuidParam "orderId"
        result <- lift $ Order.delete (OrderId oid)
        case result of
            Left err@(OrderDidNotExist _) -> httpError notFound404 err
            Right _                       -> finish
