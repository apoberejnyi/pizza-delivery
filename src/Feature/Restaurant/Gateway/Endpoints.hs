{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Feature.Restaurant.Gateway.Endpoints where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import qualified Data.Text.Lazy                as LT
import           Feature.Restaurant.Gateway.DTO
import qualified Feature.Restaurant.Contract   as Restaurant
import           Feature.Restaurant.Error
import           Feature.Restaurant.Types
import           Feature.Auth.Contract         as Auth
import qualified Feature.User.Contract         as User
import           Gateway.Auth
import           Gateway.Error
import           Gateway.Util
import           Network.HTTP.Types
import           Web.Scotty.Trans              as S

type EndpointsMonad m
  = (MonadIO m, Restaurant.Service m, Auth.Service m, User.Service m)

endpoints :: EndpointsMonad m => ScottyT LT.Text m ()
endpoints = do
  get "/api/restaurants" $ do
    allowRoles ["admin"]
    result <- lift Restaurant.getAll
    json (fmap toDTO result :: [RestaurantDto])

  get "/api/restaurants/:id" $ do
    allowRoles ["admin"]
    rid    <- uuidParam "id"
    result <- lift $ Restaurant.getById (RestaurantId rid)
    case result of
      Left err@(RestaurantNotFound _) -> httpError notFound404 err
      Right restaurant -> json (toDTO restaurant :: RestaurantDto)

  post "/api/restaurants" $ do
    allowRoles ["admin"]
    (payload :: RestaurantForCreateDto) <- parseBody
    result <- lift $ Restaurant.register (fromDTO payload)
    case result of
      Left err@(RestaurantNameAlreadyInUse _) -> httpError conflict409 err
      Right rid -> status status201 >> json (unRestaurantId rid)

  delete "/api/restaurants/:id" $ do
    allowRoles ["admin"]
    rid    <- uuidParam "id"
    result <- lift $ Restaurant.delete (RestaurantId rid)
    case result of
      Left  err@(RestaurantDidNotExist _) -> httpError conflict409 err
      Right _                             -> finish

