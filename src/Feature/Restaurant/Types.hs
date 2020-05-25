module Feature.Restaurant.Types where

import Base.Types.Coordinates (Coordinates)
import Control.Monad.IO.Class
import qualified Data.Text as T
import Data.UUID

type GetAllRestaurants m = m [Restaurant]
type GetRestaurantById m = RestaurantId -> m (Maybe Restaurant)
type RegisterRestaurant m = RestaurantForCreate -> m (Either CreateRestaurantError RestaurantId)
type DeleteRestaurant m = RestaurantId -> m (Either DeleteRestaurantError ())

class (MonadIO m) => Service m where
    getAll :: GetAllRestaurants m
    getById :: GetRestaurantById m
    register :: RegisterRestaurant m
    delete :: DeleteRestaurant m

newtype RestaurantId = RestaurantId { unRestaurantId :: UUID }

data Restaurant = Restaurant
    { restaurantId          :: RestaurantId
    , restaurantName        :: T.Text
    , restaurantCoordinates :: Coordinates
    }

data RestaurantForCreate = RestaurantForCreate T.Text Coordinates
data CreateRestaurantError = RestaurantNameAlreadyInUse

newtype DeleteRestaurantError = RestaurantNotFound RestaurantId
