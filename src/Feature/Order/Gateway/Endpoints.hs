{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Feature.Order.Gateway.Endpoints where

import Base.HTTP
import Base.Types.Address
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.Aeson hiding ( json )
import Data.List.NonEmpty
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Feature.Order.Gateway.Dto
import Feature.Order.Types
import qualified Feature.Order.Types as Order
import Feature.OrderOption.Types
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
            Nothing -> do
                status notFound404
                json $ mconcat ["Order ", show oid, " not found"]
            Just order -> json (toDTO order :: OrderDto)

    post "/api/orders" $ do
        (payload :: IffyOrderPayloadDto) <- parseBody
        result <- lift $ Order.place (fromDTO payload)
        case result of
            Right order -> do
                status created201
                json (toDTO order :: OrderDto)
            Left NoRestaurantsAvailable -> do
                status badRequest400
                json ("No restaurant is available to process the order" :: T.Text)
            Left AddressNotFound -> do
                status badRequest400
                json ("Unable to identify order address" :: T.Text)
            Left (AmbiguousAddress addresses) -> do
                status badRequest400
                json $ object
                    [ "error" .= ("Provided address is ambiguous" :: T.Text)
                    , "variants" .= (unAddress <$> addresses :: NonEmpty T.Text)
                    ]
            Left (UnknownOrderOption ooid) -> do
                status badRequest400
                json $ mconcat ["Unknown order option ", show $ unIffyOrderOptionId ooid]

    S.delete "/api/orders/:id" $ do
        oid <- uuidParam "orderId"
        result <- lift $ Order.delete (OrderId oid)
        case result of
            Left (OrderNotFound _) -> do
                status notFound404
                json $ mconcat ["Order ", show oid, " not found"]
            Right _ -> finish
