module Feature.Restaurant.Service where

import Base.Types.UUID
import Feature.Restaurant.Persistence.Contract
import Feature.Restaurant.Types

getAllRestaurants :: (Repo m) => m [Restaurant]
getAllRestaurants = queryAll

registerRestaurant :: (Repo m, UUIDGen m) => RestaurantForCreate -> m (Either CreateRestaurantError RestaurantId)
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
