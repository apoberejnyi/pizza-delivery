{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Feature.Restaurant.Service where

import           Data.Generate.UUID
import           Feature.Restaurant.Persistence.Types
                                               as Persistence
import           Feature.Restaurant.Contract   as Restaurant
import           Feature.Restaurant.Error
import           Feature.Restaurant.Types

getAllRestaurants :: (Repo m) => GetAllRestaurants m
getAllRestaurants = queryAll

getRestaurantById :: (Repo m) => GetRestaurantById m
getRestaurantById rid =
  maybe (Left $ RestaurantNotFound rid) Right <$> queryById rid

registerRestaurant :: (Repo m, UUIDGen m) => RegisterRestaurant m
registerRestaurant RestaurantForCreate {..} = do
  uuid <- nextUUID
  let rid = RestaurantId uuid
  let restaurant =
        Restaurant { id = rid, name = name, coordinates = coordinates }
  inserted <- insert restaurant
  pure $ rid <$ inserted

deleteRestaurant :: (Repo m) => Restaurant.DeleteRestaurant m
deleteRestaurant = Persistence.delete
