{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Feature.Restaurant.Types where

import Control.Error
import Data.Coordinates ( Coordinates )
import Data.Text as T
import Data.UUID

type GetAllRestaurants m = m [Restaurant]
type GetRestaurantById m = RestaurantId -> m (Either GetRestaurantError Restaurant)
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
    { restaurantName        :: Text
    , restaurantCoordinates :: Coordinates
    }

newtype CreateRestaurantError = RestaurantNameAlreadyInUse Text
instance Show CreateRestaurantError where
    show (RestaurantNameAlreadyInUse name) = mconcat ["Restaurant ", unpack name ," is already in use"]
instance Error CreateRestaurantError where
    code (RestaurantNameAlreadyInUse _) = "Restaurant_NameAlreadyInUse"

newtype DeleteRestaurantError = RestaurantDidNotExist RestaurantId
instance Show DeleteRestaurantError where
    show (RestaurantDidNotExist rid) = mconcat ["Restaurant ", show rid, " not found"]
instance Error DeleteRestaurantError where
    code (RestaurantDidNotExist _) = "Restaurant_NotFound"

newtype GetRestaurantError = RestaurantNotFound RestaurantId
instance Show GetRestaurantError where
    show (RestaurantNotFound rid) = mconcat ["Restaurant ", show rid, " not found"]
instance Error GetRestaurantError where
    code (RestaurantNotFound _) = "Restaurant_NotFound"

