{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Client.OpenCage
  ( Client.OpenCage.resolveAddress
  , OpenCageConfig(..)
  )
where

import           Data.Address
import           Data.Aeson
import           Data.Has
import           Data.Coordinates
import           Data.Text
import           GHC.Generics
import           Network.HTTP.Req
import           System.Envy
import           Control.Monad.Reader

resolveAddress
  :: (MonadHttp m, Has OpenCageConfig r, MonadReader r m)
  => IffyAddress
  -> m [Location]
resolveAddress (IffyAddress address) = do
  OpenCageConfig {..} <- asks getter
  let url = https "api.opencagedata.com" /: "geocode" /: "v1" /: "json"
  let query = mconcat
        [ "q" =: address
        , "key" =: apiKey
        , "no_annotations" =: ("1" :: Text)
        , "language" =: ("ru" :: Text)
        , "countrycode" =: ("by" :: Text)
        ]
  response <- req GET url NoReqBody jsonResponse query
  pure $ pickAddresses $ responseBody response

pickAddresses :: ForwardGeocodingResponse -> [(Address, Coordinates)]
pickAddresses res = mapResult <$> results res
 where
  mapResult (GeocodingResult geometry address) =
    (Address address, asCoordinates geometry)
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

newtype OpenCageConfig = OpenCageConfig
    { apiKey :: Text
    }

instance FromEnv OpenCageConfig where
  fromEnv _ = OpenCageConfig <$> env "OPEN_CAGE_API_KEY"
