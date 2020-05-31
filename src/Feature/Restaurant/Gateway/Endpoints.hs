{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Feature.Restaurant.Gateway.Endpoints where

import Base.HTTP
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Feature.Restaurant.Gateway.Dto
import Feature.Restaurant.Types as Restaurant
import Network.HTTP.Types
import Web.Scotty.Trans as S

endpoints :: (MonadIO m, Restaurant.Service m) => ScottyT LT.Text m ()
endpoints = do
    get "/api/restaurants" $ do
        result <- lift Restaurant.getAll
        json (fmap toDTO result :: [RestaurantDto])

    get "/api/restaurants/:id" $ do
        rid <- uuidParam "id"
        result <- lift $ Restaurant.getById (RestaurantId rid)
        case result of
            Nothing -> do
                status notFound404
                json $ mconcat ["Restaurant ", show rid, " not found"]
            Just restaurant -> json (toDTO restaurant :: RestaurantDto)

    post "/api/restaurants" $ do
        (payload :: RestaurantForCreateDto) <- parseBody
        result <- lift $ Restaurant.register (fromDTO payload)
        case result of
            Right rid -> do
                status status201
                json $ unRestaurantId rid
            Left RestaurantNameAlreadyInUse -> do
                status conflict409
                json ("Restaurant name is already in use" :: T.Text)

    S.delete "/api/restaurants/:id" $ do
        rid <- uuidParam "id"
        result <- lift $ Restaurant.delete (RestaurantId rid)
        case result of
            Left (RestaurantNotFound _) -> do
                status notFound404
                json $ mconcat ["Restaurant ", show rid, " not found"]
            Right _ -> finish

