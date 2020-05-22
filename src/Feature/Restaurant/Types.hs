{-# LANGUAGE DeriveGeneric #-}

module Feature.Restaurant.Types
    ( Restaurant(..)
    , RestaurantId(..)
    , RestaurantForCreate(..)
    , CreateRestaurantError(..)
    , DeleteRestaurantError(..)
    ) where

import Base.Types.Coordinates (Coordinates)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Text as T
import Data.UUID
import GHC.Generics (Generic)

newtype RestaurantId = RestaurantId UUID deriving (Eq, Generic)
instance FromJSON RestaurantId
instance ToJSON RestaurantId

data Restaurant = Restaurant
    { restaurantId          :: RestaurantId
    , restaurantName        :: T.Text
    , restaurantCoordinates :: Coordinates
    }

data RestaurantForCreate = RestaurantForCreate T.Text Coordinates
data CreateRestaurantError = RestaurantNameAlreadyInUse

data DeleteRestaurantError = RestaurantNotFound RestaurantId
