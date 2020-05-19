{-# LANGUAGE DeriveGeneric #-}

module Feature.Restaurant.Types
    ( Restaurant(..)
    , RestaurantId(..)
    , GetAllRestaurants
    ) where

import Base.Types.Coordinates (Coordinates)
import Data.Aeson (FromJSON, ToJSON)
import Data.List.NonEmpty (NonEmpty)
import GHC.Generics (Generic)

newtype RestaurantId = RestaurantId String deriving (Eq, Generic)
instance FromJSON RestaurantId
instance ToJSON RestaurantId

data Restaurant = Restaurant
    { restaurantId          :: RestaurantId
    , restaurantCoordinates :: Coordinates
    }

type GetAllRestaurants m = m (NonEmpty Restaurant)
