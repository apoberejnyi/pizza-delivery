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
import Control.Monad
import Data.Either.Combinators
import Data.List.NonEmpty as NEL
import Data.UUID
import qualified Feature.Order.Persistence.Contract as Order
import Feature.Order.Types
import qualified Feature.OrderOption.Contract as OrderOption
import Feature.OrderOption.Types
import qualified Feature.Restaurant.Contract as Restaurant
import Feature.Restaurant.Types

getAllOrders :: (Order.Repo m) => m [Order]
getAllOrders = Order.queryAll

type PlaceOrder m =
    ( UUIDGen m
    , Concurrent m
    , Restaurant.Service m
    , OrderOption.Service m
    , AddressResolver m
    , Order.Repo m
    )

placeOrder :: PlaceOrder m => IffyOrderPayload -> m (Either PlaceOrderError Order)
placeOrder IffyOrderPayload{..} = do
    uuid <- nextUUID
    (rs, addresses, items') <- concurrently3
        Restaurant.getAll
        (resolveAddress orderPayloadAddress)
        (validateOrderPayloadItems orderPayloadItems)

    let restaurants = maybeToRight NoRestaurantsAvailable (nonEmpty rs)
    let order = join $ mkOrder uuid addresses <$> restaurants <*> items'

    mapM_ Order.insert order
    pure order

validateOrderPayloadItems :: (OrderOption.Service m) =>
    NonEmpty IffyOrderOptionId ->
    m (Either PlaceOrderError (NonEmpty OrderOptionId))
validateOrderPayloadItems items = do
    checks <- OrderOption.checkExistence items
    pure $ sequence $ validatePair <$> NEL.zip checks items
        where
    validatePair (ooid', iffyOoid) = maybe (Left $ UnknownOrderOption iffyOoid) (Right) ooid'

mkOrder :: UUID -> [(Address, Coordinates)] -> NonEmpty Restaurant -> NonEmpty OrderOptionId -> Either PlaceOrderError Order
mkOrder uuid locations' restaurants ooids = case nonEmpty locations' of
    Nothing -> Left AddressNotFound
    Just locations -> if NEL.length addresses > 1
        then Left $ AmbiguousAddress addresses
        else Right $ Order
            { orderId = OrderId uuid
            , orderPayload = OrderPayload
                { orderPayloadItems = ooids
                , orderPayloadAddress = NEL.head addresses
                }
            , orderRestaurantId = restaurantId closestRestaurant
            }
            where
        (addresses, coordinates) = NEL.unzip locations
        closestRestaurant = getClosestRestaurant restaurants $ NEL.head coordinates


getClosestRestaurant :: NonEmpty Restaurant -> Coordinates -> Restaurant
getClosestRestaurant restaurants coordinates = fst closest where
    closest         = foldr pickClosest x xs
    (x :| xs) = appendDistance coordinates <$> restaurants

type RestaurantDistance = (Restaurant, Distance)

pickClosest :: RestaurantDistance -> RestaurantDistance -> RestaurantDistance
pickClosest r1@(_, d1) r2@(_, d2) = if d1 < d2 then r1 else r2

appendDistance :: Coordinates -> Restaurant -> (Restaurant, Distance)
appendDistance c1 r@Restaurant{..} = (r, getDistance c1 restaurantCoordinates)
