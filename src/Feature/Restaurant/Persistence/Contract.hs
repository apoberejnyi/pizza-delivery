module Feature.Restaurant.Persistence.Contract where

import Feature.Restaurant.Types

class Monad m => Repo m where
    queryAll :: m [Restaurant]
    queryById :: RestaurantId -> m (Maybe Restaurant)
    insert :: Restaurant -> m (Either CreateRestaurantError ())
    delete :: RestaurantId -> m (Either DeleteRestaurantError ())
