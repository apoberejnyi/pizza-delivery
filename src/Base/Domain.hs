module Base.Domain where

newtype Address = Address String

type Latitude = Double
type Longitude = Double
data Coordinates = Coordinates Latitude Longitude
