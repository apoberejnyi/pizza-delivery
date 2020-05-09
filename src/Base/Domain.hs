{-# LANGUAGE DeriveGeneric #-}

module Base.Domain where

import           Data.Aeson
import           Data.Text
import           GHC.Generics

newtype Address = Address Text deriving (Show, Generic)
instance FromJSON Address
instance ToJSON Address

type Latitude = Double
type Longitude = Double
data Coordinates = Coordinates Latitude Longitude
