{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Feature.Restaurant.Gateway.Endpoints where

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import qualified Data.Text.Lazy as LT
import Feature.Restaurant.Gateway.DTO
import Feature.Restaurant.Types as Restaurant
import Gateway.Error
import Gateway.Util
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
            Left err@(RestaurantNotFound _)        -> httpError notFound404 err
            Right restaurant -> json (toDTO restaurant :: RestaurantDto)

    post "/api/restaurants" $ do
        (payload :: RestaurantForCreateDto) <- parseBody
        result <- lift $ Restaurant.register (fromDTO payload)
        case result of
            Left err@(RestaurantNameAlreadyInUse _) -> httpError conflict409 err
            Right rid -> status status201 >> json (unRestaurantId rid)

    S.delete "/api/restaurants/:id" $ do
        rid <- uuidParam "id"
        result <- lift $ Restaurant.delete (RestaurantId rid)
        case result of
            Left err@(RestaurantDidNotExist _) -> httpError conflict409 err
            Right _                            -> finish

