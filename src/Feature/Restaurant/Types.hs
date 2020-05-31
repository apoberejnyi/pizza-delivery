{-# LANGUAGE DuplicateRecordFields #-}

module Feature.Restaurant.Types where

import Base.Types.Coordinates ( Coordinates )
import qualified Data.Text as T
import Data.UUID

type GetAllRestaurants m = m [Restaurant]
type GetRestaurantById m = RestaurantId -> m (Maybe Restaurant)
type RegisterRestaurant m = RestaurantForCreate -> m (Either CreateRestaurantError RestaurantId)
type DeleteRestaurant m = RestaurantId -> m (Either DeleteRestaurantError ())

class (Monad m) => Service m where
    getAll :: GetAllRestaurants m
    getById :: GetRestaurantById m
    register :: RegisterRestaurant m
    delete :: DeleteRestaurant m

newtype RestaurantId = RestaurantId { unRestaurantId :: UUID } deriving (Eq, Show)

data Restaurant = Restaurant
    { restaurantId          :: RestaurantId
    , restaurantName        :: T.Text
    , restaurantCoordinates :: Coordinates
    }
    deriving (Eq)

data RestaurantForCreate = RestaurantForCreate
    { restaurantName        :: T.Text
    , restaurantCoordinates :: Coordinates
    }

data CreateRestaurantError = RestaurantNameAlreadyInUse
newtype DeleteRestaurantError = RestaurantNotFound RestaurantId
