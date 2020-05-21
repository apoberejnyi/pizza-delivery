module Feature.Restaurant.Persistence.Contract where

import Feature.Restaurant.Types

class Monad m => Repo m where
    queryAll :: m [Restaurant]
    insert :: Restaurant -> m (Either CreateRestaurantError ())
