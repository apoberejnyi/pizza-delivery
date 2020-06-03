{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module Feature.Restaurant.Gateway.DTO where

import           Data.Aeson
import           Data.Coordinates
import           Data.Text
import           Data.UUID
import           Feature.Restaurant.Types
import           Gateway.Util
import           GHC.Generics
import           Prelude                 hiding ( id )

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
  toDTO Restaurant {..} = RestaurantDto { id   = unRestaurantId id
                                        , name = name
                                        , lat  = lat'
                                        , lon  = lon'
                                        }
    where Coordinates lat' lon' = coordinates

instance FromDTO RestaurantForCreateDto RestaurantForCreate where
  fromDTO RestaurantForCreateDto {..} =
    RestaurantForCreate { name = name, coordinates = Coordinates lat lon }
