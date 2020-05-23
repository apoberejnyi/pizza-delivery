module Feature.Restaurant.Types
    ( Restaurant(..)
    , RestaurantId(..)
    , RestaurantForCreate(..)
    , CreateRestaurantError(..)
    , DeleteRestaurantError(..)
    ) where

import Base.Types.Coordinates (Coordinates)
import qualified Data.Text as T
import Data.UUID

newtype RestaurantId = RestaurantId { unRestaurantId :: UUID }

data Restaurant = Restaurant
    { restaurantId          :: RestaurantId
    , restaurantName        :: T.Text
    , restaurantCoordinates :: Coordinates
    }

data RestaurantForCreate = RestaurantForCreate T.Text Coordinates
data CreateRestaurantError = RestaurantNameAlreadyInUse

data DeleteRestaurantError = RestaurantNotFound RestaurantId
