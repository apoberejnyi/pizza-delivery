{-# LANGUAGE OverloadedStrings #-}

module Feature.Restaurant.Gateway.Dto where

import Base.Types.Coordinates
import Control.Applicative
import Data.Aeson
import Feature.Restaurant.Types

newtype RestaurantDto = RestaurantDto Restaurant

instance ToJSON RestaurantDto where
    toJSON (RestaurantDto (Restaurant (RestaurantId rid) name (Coordinates lat lon))) = object
        [ "id" .= rid
        , "name" .= name
        , "lat" .= lat
        , "lon" .= lon
        ]

newtype RestaurantForCreateDto = RestaurantForCreateDto RestaurantForCreate

instance FromJSON RestaurantForCreateDto where
    parseJSON = withObject "RestaurantForCreateDto" $ \v -> do
        name <- v .: "name"
        coordinates <- liftA2 Coordinates (v .: "lat") (v .: "lon")
        let restaurant = RestaurantForCreate name coordinates
        pure $ RestaurantForCreateDto restaurant
