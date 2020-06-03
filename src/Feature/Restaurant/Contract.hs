{-# LANGUAGE DuplicateRecordFields #-}

module Feature.Restaurant.Contract where

import           Feature.Restaurant.Error
import           Feature.Restaurant.Types

type GetAllRestaurants m = m [Restaurant]
type GetRestaurantById m
  = RestaurantId -> m (Either GetRestaurantError Restaurant)
type RegisterRestaurant m
  = RestaurantForCreate -> m (Either CreateRestaurantError RestaurantId)
type DeleteRestaurant m = RestaurantId -> m (Either DeleteRestaurantError ())

class (Monad m) => Service m where
    getAll :: GetAllRestaurants m
    getById :: GetRestaurantById m
    register :: RegisterRestaurant m
    delete :: DeleteRestaurant m
