{-# LANGUAGE DeriveGeneric #-}

module Restaurant where

import Base.Coordinates (Coordinates)
import Data.Aeson
import Data.List.NonEmpty (NonEmpty)
import GHC.Generics

newtype RestaurantId = RestaurantId String deriving (Eq, Generic)
instance FromJSON RestaurantId
instance ToJSON RestaurantId

data Restaurant = Restaurant
    { restaurantId :: RestaurantId
    , coordinates  :: Coordinates
    }

type GetAllRestaurants m = m (NonEmpty Restaurant)
