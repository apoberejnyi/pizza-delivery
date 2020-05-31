{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module Feature.Restaurant.Gateway.Dto where

import Base.HTTP
import Base.Types.Coordinates
import Data.Aeson
import Data.Text
import Data.UUID
import Feature.Restaurant.Types
import GHC.Generics
import Prelude hiding ( id )

instance ToJSON RestaurantDto
data RestaurantDto = RestaurantDto
    { id   :: UUID
    , name :: Text
    , lat  :: Double
    , lon  :: Double
    }
    deriving (Generic)

instance FromJSON RestaurantForCreateDto
data RestaurantForCreateDto = RestaurantForCreateDto
    { id   :: UUID
    , name :: Text
    , lat  :: Double
    , lon  :: Double
    }
    deriving (Generic)

instance ToDTO RestaurantDto Restaurant where
    toDTO Restaurant{..} = RestaurantDto
        { id = unRestaurantId restaurantId
        , name = restaurantName
        , lat = lat'
        , lon = lon'
        }
            where
        Coordinates lat' lon' = restaurantCoordinates

instance FromDTO RestaurantForCreateDto RestaurantForCreate where
    fromDTO RestaurantForCreateDto{..} = RestaurantForCreate
        { restaurantName = name
        , restaurantCoordinates = Coordinates lat lon
        }
