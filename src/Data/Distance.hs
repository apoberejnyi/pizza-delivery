module Data.Distance
  ( Distance
  , getDistance
  )
where

import Data.Coordinates ( Coordinates (Coordinates) )

type Distance = Double

-- https://stackoverflow.com/questions/365826/calculate-distance-between-2-gps3-coordinates
getDistance :: Coordinates -> Coordinates -> Distance
getDistance (Coordinates lat1 lon1) (Coordinates lat2 lon2) = earthRadiusKm * c
 where
  earthRadiusKm = 6371

  dLat = toRadians (lat2 - lat1)
  dLon = toRadians (lon2 - lon1)

  lat1Radians = toRadians lat1
  lat2Radians = toRadians lat2

  a =
    sin (dLat / 2)
      * sin (dLat / 2)
      + sin (dLon / 2)
      * sin (dLon / 2)
      * cos lat1Radians
      * cos lat2Radians

  c = 2 * atan2 (sqrt a) (sqrt 1 - a)

toRadians :: Floating a => a -> a
toRadians degrees = degrees * pi / 180
