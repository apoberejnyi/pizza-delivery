module Order.Service (placeOrder) where

import Base.Coordinates (Coordinates)
import Base.Distance (Distance, getDistance)
import Base.ResolveAddress (ResolveAddress)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Order
    ( PlaceOrder
    , ProcessOrderRequest (ProcessOrderRequest)
    , ProcessOrderRequestId
    , orderAddress
    , requestId
    , requestOrder
    , requestRestaurantId
    )
import Restaurant (GetAllRestaurants, Restaurant (Restaurant), restaurantId)

placeOrder
  :: Monad m
  => ResolveAddress m
  -> GetAllRestaurants m
  -> m ProcessOrderRequestId
  -> PlaceOrder m
placeOrder getCoordinates getRestaurants generateRequestId order = do
  coordinates <- getCoordinates (orderAddress order)
  restaurants <- getRestaurants
  rId <- generateRequestId
  let closestRestaurant = getClosestRestaurant restaurants coordinates
  pure $ ProcessOrderRequest
    { requestId = rId
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
