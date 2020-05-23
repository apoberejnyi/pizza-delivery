{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RecordWildCards #-}

module Feature.Order.Service
    ( getAllOrders
    , placeOrder
    ) where

import Base.Concurrency
import Base.Types.Address
import Base.Types.Coordinates
import Base.Types.Distance
import Base.Types.UUID
import Control.Applicative
import Data.Bifunctor
import Data.Either.Combinators
import Data.List.NonEmpty
import Data.UUID
import qualified Feature.Order.Persistence.Contract as Order
import Feature.Order.Types
import qualified Feature.Restaurant.Contract as Restaurant
import Feature.Restaurant.Types

getAllOrders :: (Order.Repo m) => m [Order]
getAllOrders = Order.queryAll

-- TODO: Validate items existence
type PlaceOrder m = (UUIDGen m, Concurrent m, Restaurant.Service m, AddressResolver m, Order.Repo m)
placeOrder :: PlaceOrder m => OrderPayload -> m (Either PlaceOrderError Order)
placeOrder payload@OrderPayload{..} = do
    uuid <- nextUUID
    (rs, cs) <- concurrently Restaurant.getAll (resolveAddress orderPayloadAddress)

    let restaurants = maybeToRight NoRestaurantsAvailable (nonEmpty rs)
    let coordinates = first OnAddressResolution cs
    let order = uncurry (mkOrder uuid payload) <$> liftA2 (,) restaurants coordinates

    mapM_ Order.insert order
    pure order

mkOrder :: UUID -> OrderPayload -> NonEmpty Restaurant -> Coordinates -> Order
mkOrder uuid payload rs c = Order
    { orderId = OrderId uuid
    , orderPayload = payload
    , orderRestaurantId = restaurantId (getClosestRestaurant rs c)
    }

getClosestRestaurant :: NonEmpty Restaurant -> Coordinates -> Restaurant
getClosestRestaurant restaurants coordinates = fst closest where
    closest         = foldr pickClosest x xs
    (x :| xs) = appendDistance coordinates <$> restaurants

type RestaurantDistance = (Restaurant, Distance)

pickClosest :: RestaurantDistance -> RestaurantDistance -> RestaurantDistance
pickClosest r1@(_, d1) r2@(_, d2) = if d1 < d2 then r1 else r2

appendDistance :: Coordinates -> Restaurant -> (Restaurant, Distance)
appendDistance c1 r@Restaurant{..} = (r, getDistance c1 restaurantCoordinates)
