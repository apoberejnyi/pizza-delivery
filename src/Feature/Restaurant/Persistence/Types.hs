module Feature.Restaurant.Persistence.Types where

import           Feature.Restaurant.Error
import           Feature.Restaurant.Types

type QueryAllRestaurants m = m [Restaurant]
type QueryRestaurantById m = RestaurantId -> m (Maybe Restaurant)
type InsertRestaurant m = Restaurant -> m (Either CreateRestaurantError ())
type DeleteRestaurant m = RestaurantId -> m (Either DeleteRestaurantError ())

class Monad m => Repo m where
    queryAll :: QueryAllRestaurants m
    queryById :: QueryRestaurantById m
    insert :: InsertRestaurant m
    delete :: Feature.Restaurant.Persistence.Types.DeleteRestaurant m
