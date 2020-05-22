{-# LANGUAGE OverloadedStrings #-}

module Feature.Restaurant.Gateway.Endpoints where

import Base.HTTP
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Feature.Restaurant.Contract as Restaurant
import Feature.Restaurant.Gateway.Dto
import Feature.Restaurant.Types
import Network.HTTP.Types
import Web.Scotty.Trans

endpoints :: (MonadIO m, Restaurant.Service m) => ScottyT LT.Text m ()
endpoints = do
    get "/api/restaurants" $ do
        result <- lift Restaurant.getAll
        json $ map RestaurantDto result

    get "/api/restaurants/:id" $ do
        rid <- uuidParam "id"
        result <- lift $ Restaurant.getById (RestaurantId rid)
        case result of
            Nothing -> do
                status notFound404
                json $ mconcat ["Restaurant ", show rid, " not found"]
            Just restaurant -> json $ RestaurantDto restaurant

    post "/api/restaurants" $ do
        RestaurantForCreateDto payload <- parseBody
        result <- lift $ Restaurant.register payload
        case result of
            Right ooid -> do
                status status201
                json ooid
            Left RestaurantNameAlreadyInUse -> do
                status conflict409
                json ("Restaurant name is already in use" :: T.Text)

    delete "/api/restaurants/:id" $ do
        rid <- uuidParam "id"
        result <- lift $ Restaurant.delete (RestaurantId rid)
        case result of
            Left (RestaurantNotFound _) -> do
                status notFound404
                json $ mconcat ["Restaurant ", show rid, " not found"]
            Right _ -> finish

