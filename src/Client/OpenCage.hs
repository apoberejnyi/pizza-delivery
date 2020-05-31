{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Client.OpenCage
    ( Client.OpenCage.resolveAddress
    , OpenCageApiEnv(..)
    ) where

import Data.Address
import Data.Aeson
import Data.Coordinates
import Data.Text
import GHC.Generics
import Network.HTTP.Req
import System.Envy

resolveAddress :: (MonadHttp m) => OpenCageApiEnv -> IffyAddress -> m [(Address, Coordinates)]
resolveAddress OpenCageApiEnv{..} (IffyAddress address) = do
    let url = https "api.opencagedata.com" /: "geocode" /: "v1" /: "json"
    response <- req GET url NoReqBody jsonResponse $
        "q" =: address <>
        "key" =: apiKey <>
        "no_annotations" =: ("1" :: Text) <>
        "language" =: ("ru" :: Text) <>
        "countrycode" =: ("by" :: Text)
    pure $ pickAddresses $ responseBody response

pickAddresses :: ForwardGeocodingResponse -> [(Address, Coordinates)]
pickAddresses res = mapResult <$> results res
        where
    mapResult (GeocodingResult geometry address) = (Address address, asCoordinates geometry)
    asCoordinates (Geometry lat lon) = Coordinates lat lon

instance FromJSON ForwardGeocodingResponse
newtype ForwardGeocodingResponse = ForwardGeocodingResponse
    { results :: [GeocodingResult]
    }
    deriving (Show, Generic)

instance FromJSON GeocodingResult
data GeocodingResult = GeocodingResult
    { geometry  :: Geometry
    , formatted :: Text
    }
    deriving (Show, Generic)

instance FromJSON Geometry
data Geometry = Geometry
    { lat :: Double
    , lng :: Double
    }
    deriving (Show, Generic)

newtype OpenCageApiEnv = OpenCageApiEnv
    { apiKey :: Text
    }

instance FromEnv OpenCageApiEnv where
  fromEnv _ = OpenCageApiEnv <$> env "OPEN_CAGE_API_KEY"
