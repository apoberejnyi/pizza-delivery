module Feature.Restaurant.Contract where

import Feature.Restaurant.Types

class Monad m => Service m where
    getAll :: m [Restaurant]
    register :: RestaurantForCreate -> m (Either CreateRestaurantError RestaurantId)
