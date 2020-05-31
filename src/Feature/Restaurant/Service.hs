{-# LANGUAGE DuplicateRecordFields #-}

module Feature.Restaurant.Service where

import Base.Types.UUID
import Feature.Restaurant.Persistence.Types as Persistence
import Feature.Restaurant.Types as Restaurant

getAllRestaurants :: (Repo m) => GetAllRestaurants m
getAllRestaurants = queryAll

getRestaurantById :: (Repo m) => GetRestaurantById m
getRestaurantById = queryById

registerRestaurant :: (Repo m, UUIDGen m) => RegisterRestaurant m
registerRestaurant (RestaurantForCreate name coordinates) = do
    uuid <- nextUUID
    let rid = RestaurantId uuid
    let restaurant = Restaurant
            { restaurantId = rid
            , restaurantName = name
            , restaurantCoordinates = coordinates
            }
    inserted <- insert restaurant
    pure $ rid <$ inserted

deleteRestaurant :: (Repo m) => Restaurant.DeleteRestaurant m
deleteRestaurant = Persistence.delete
