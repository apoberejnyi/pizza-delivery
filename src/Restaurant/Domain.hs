module Restaurant.Domain where

import           Base.Domain        (Coordinates)
import           Data.List.NonEmpty (NonEmpty)

newtype RestaurantId = RestaurantId String

data Restaurant = Restaurant
    { restaurantId :: RestaurantId
    , coordinates  :: Coordinates
    }

type GetAllRestaurants m = m (NonEmpty Restaurant)
