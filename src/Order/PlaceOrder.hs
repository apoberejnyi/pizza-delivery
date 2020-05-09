module Order.PlaceOrder where

import           Base.Distance
import           Base.Domain

import           Base.ResolveAddress
import           Data.List.NonEmpty
import           Data.UUID
import qualified Data.UUID.V4        as UUID
import           Order.Domain
import           Restaurant.Domain

placeOrder
  :: Monad m
  => ResolveAddress m
  -> GetAllRestaurants m
  -> ProcessOrderRequestId
  -> PlaceOrder m

placeOrder getCoordinates getRestaurants requestId order = do
  coordinates <- getCoordinates (orderAddress order)
  restaurants <- getRestaurants
  let closestRestaurant = getClosestRestaurant restaurants coordinates
  pure $ ProcessOrderRequest
    { requestId = requestId
    , requestOrder = order
    , requestRestaurantId = restaurantId closestRestaurant
    }

getClosestRestaurant :: NonEmpty Restaurant -> Coordinates -> Restaurant
getClosestRestaurant restaurants coordinates = fst closest
 where
  closest         = foldr pickClosest first rest
  (first :| rest) = appendDistance coordinates <$> restaurants

type RestaurantDistance = (Restaurant, Distance)

pickClosest :: RestaurantDistance -> RestaurantDistance -> RestaurantDistance
pickClosest r1@(_, d1) r2@(_, d2) = if d1 < d2 then r1 else r2

appendDistance :: Coordinates -> Restaurant -> (Restaurant, Distance)
appendDistance c1 r@(Restaurant _ c2) = (r, getDistance c1 c2)
