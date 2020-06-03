{-# LANGUAGE DuplicateRecordFields #-}

module Feature.Restaurant.Types where

import           Data.Coordinates               ( Coordinates )
import           Data.Text                     as T
import           Data.UUID

newtype RestaurantId = RestaurantId { unRestaurantId :: UUID } deriving (Eq, Show)

data Restaurant = Restaurant
    { id          :: RestaurantId
    , name        :: T.Text
    , coordinates :: Coordinates
    }
    deriving (Eq)

data RestaurantForCreate = RestaurantForCreate
    { name        :: Text
    , coordinates :: Coordinates
    }
