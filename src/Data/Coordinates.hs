module Data.Coordinates where

type Latitude = Double
type Longitude = Double
data Coordinates = Coordinates Latitude Longitude
    deriving (Eq)
