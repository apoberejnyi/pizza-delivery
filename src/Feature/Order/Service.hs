{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RecordWildCards #-}

module Feature.Order.Service
    ( getAllOrders
    , getOrderById
    , placeOrder
    , deleteOrder
    ) where

import Control.Concurrency
import Control.Monad
import Data.Address
import Data.Coordinates
import Data.Distance
import Data.Either.Combinators
import Data.List.NonEmpty as NEL
import Data.UUID
import qualified Feature.Order.Persistence.Types as Order
import Feature.Order.Types
import Feature.OrderOption.Types as OrderOption
import Feature.Restaurant.Types as Restaurant
import Persistence.UUID

getAllOrders :: (Order.Repo m) => GetAllOrders m
getAllOrders = Order.queryAll

getOrderById :: (Order.Repo m) => GetOrderById m
getOrderById oid = maybe (Left $ OrderNotFound oid) Right <$> Order.queryById oid

deleteOrder :: (Order.Repo m) => DeleteOrder m
deleteOrder = Order.delete

type PlaceOrderM m =
    ( UUIDGen m
    , Concurrent m
    , Restaurant.Service m
    , OrderOption.Service m
    , AddressResolver m
    , Order.Repo m
    )

placeOrder :: PlaceOrderM m => PlaceOrder m
placeOrder IffyOrderPayload{..} = do
    uuid <- nextUUID
    (restaurants', addresses, items) <- concurrently3
        Restaurant.getAll
        (resolveAddress iffyOrderPayloadAddress)
        (validateOrderPayloadItems iffyOrderPayloadItems)

    let restaurants = maybeToRight NoRestaurantsAvailable (nonEmpty restaurants')
    let order = join $ mkOrder uuid addresses <$> restaurants <*> items

    mapM_ Order.insert order
    pure order

validateOrderPayloadItems :: (OrderOption.Service m) =>
    NonEmpty IffyOrderOptionId ->
    m (Either PlaceOrderError (NonEmpty OrderOptionId))
validateOrderPayloadItems items = do
    checks <- OrderOption.checkExistence items
    pure $ sequence $ validatePair <$> NEL.zip checks items
  where
    validatePair (ooid', iffyOoid) = maybe (Left $ UnknownOrderOption iffyOoid) Right ooid'

mkOrder :: UUID -> [Location] -> NonEmpty Restaurant -> NonEmpty OrderOptionId -> Either PlaceOrderError Order
mkOrder uuid locations' restaurants ooids = case nonEmpty locations' of
    Nothing -> Left AddressNotFound
    Just locations -> if NEL.length addresses > 1
        then Left $ AmbiguousAddress addresses
        else Right $ Order
            { orderId = OrderId uuid
            , orderStatus = Verification
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
