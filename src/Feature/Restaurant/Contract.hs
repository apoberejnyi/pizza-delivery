module Feature.Restaurant.Contract where

import Control.Monad.IO.Class
import Feature.Restaurant.Types

class (MonadIO m) => Service m where
    getAll :: m [Restaurant]
    getById :: RestaurantId -> m (Maybe Restaurant)
    register :: RestaurantForCreate -> m (Either CreateRestaurantError RestaurantId)
    delete :: RestaurantId -> m (Either DeleteRestaurantError ())
