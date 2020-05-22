{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RecordWildCards #-}

module Feature.Order.Service
    ( placeOrder
    ) where

import Base.Concurrency
import Base.Types.Address
import Base.Types.Coordinates
import Base.Types.Distance
import Base.Types.UUID
import Control.Monad.IO.Class
import Data.List.NonEmpty
import Feature.Order.Types
import qualified Feature.Restaurant.Contract as Restaurant
import Feature.Restaurant.Types

type PlaceOrder m = (UUIDGen m, MonadIO m, Concurrent m, Restaurant.Service m, AddressResolver m)

placeOrder :: PlaceOrder m => Order -> m (Either PlaceOrderError ProcessOrderRequest)
placeOrder order@Order{..} = do
    (restaurants, coordinates) <- concurrently Restaurant.getAll (resolveAddress orderAddress)
    case nonEmpty restaurants of
        Nothing -> pure $ Left NoRestaurantsAvailable
        Just rs -> do
            rid <- nextUUID
            let closestRestaurant = getClosestRestaurant rs coordinates
            pure $ Right $ ProcessOrderRequest
                { requestId = ProcessOrderRequestId rid
                , requestOrder = order
                , requestRestaurantId = restaurantId closestRestaurant
                }

getClosestRestaurant :: NonEmpty Restaurant -> Coordinates -> Restaurant
getClosestRestaurant restaurants coordinates = fst closest where
    closest         = foldr pickClosest first rest
    (first :| rest) = appendDistance coordinates <$> restaurants

type RestaurantDistance = (Restaurant, Distance)

pickClosest :: RestaurantDistance -> RestaurantDistance -> RestaurantDistance
pickClosest r1@(_, d1) r2@(_, d2) = if d1 < d2 then r1 else r2

appendDistance :: Coordinates -> Restaurant -> (Restaurant, Distance)
appendDistance c1 r@Restaurant{..} = (r, getDistance c1 restaurantCoordinates)
