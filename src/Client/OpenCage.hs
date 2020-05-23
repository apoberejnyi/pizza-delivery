{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Client.OpenCage
    ( Client.OpenCage.resolveAddress
    , ResolveAddressError
    , OpenCageApiEnv(..)
    ) where

import Base.Types.Address
import Base.Types.Coordinates
import Control.Monad.Catch
import Data.Aeson
import Data.Text
import GHC.Generics
import Network.HTTP.Req
import System.Envy

resolveAddress :: (MonadHttp m, MonadThrow m) => OpenCageApiEnv -> Address -> m (Either ResolveAddressError Coordinates)
resolveAddress OpenCageApiEnv{..} (Address address) = do
    let url = https "api.opencagedata.com" /: "geocode" /: "v1" /: "json"
    response <- req GET url NoReqBody jsonResponse $
        "q" =: address <>
        "key" =: apiKey <>
        "no_annotations" =: ("1" :: Text) <>
        "countrycode" =: ("by" :: Text)

    let result = case pickCoordinates $ responseBody response of
            []                 -> Left NoCoordinatesFound
            [(coordinates, _)] -> Right coordinates
            xs                 -> Left $ AmbiguousCoordinates (snd <$> xs)
    pure result

-- TODO: pull the "category" component only
-- TODO: Move ambiguity handling into the consuming code
pickCoordinates :: ForwardGeocodingResponse -> [(Coordinates, Address)]
pickCoordinates res = mapResult <$> results res
        where
    mapResult (GeocodingResult geometry address) = (asCoordinates geometry, Address address)
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
