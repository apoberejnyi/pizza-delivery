{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Feature.Order.Service
  ( getAllOrders
  , getOrderById
  , placeOrder
  , deleteOrder
  )
where

import           Control.Concurrency
import           Control.Monad
import           Data.Address
import           Data.Coordinates
import           Data.Distance
import           Data.Either.Combinators
import           Data.List.NonEmpty            as NEL
import           Data.Time.Clock
import           Data.UUID
import           Data.Generate.UUID
import           Data.Generate.UTCTime
import qualified Feature.Order.Persistence.Types
                                               as Order
import           Feature.Order.Contract
import           Feature.Order.Error
import           Feature.Order.Types
import           Feature.OrderOption.Contract  as OrderOption
import           Feature.OrderOption.Types
import           Feature.Restaurant.Contract   as Restaurant
import           Feature.Restaurant.Types
import           Feature.User.Types
import           Prelude                 hiding ( id )

getAllOrders :: (Order.Repo m) => GetAllOrders m
getAllOrders = Order.queryAll

getOrderById :: (Order.Repo m) => GetOrderById m
getOrderById oid =
  maybe (Left $ OrderNotFound oid) Right <$> Order.queryById oid

deleteOrder :: (Order.Repo m) => DeleteOrder m
deleteOrder = Order.delete

type PlaceOrderMonad m
  = ( UUIDGen m
    , Concurrent m
    , Restaurant.Service m
    , OrderOption.Service m
    , AddressResolver m
    , Order.Repo m
    , UTCTimeGen m
    )

placeOrder :: PlaceOrderMonad m => PlaceOrder m
placeOrder userId IffyOrderPayload {..} = do
  uuid                              <- nextUUID
  (restaurants', addresses, items') <- concurrently3
    Restaurant.getAll
    (resolveAddress address)
    (validateOrderPayloadItems items)
  now <- currentTime

  let restaurants = maybeToRight NoRestaurantsAvailable (nonEmpty restaurants')
  let order =
        join $ mkOrder uuid addresses userId now <$> restaurants <*> items'

  mapM_ Order.insert order
  pure order

validateOrderPayloadItems
  :: (OrderOption.Service m)
  => NonEmpty IffyOrderOptionId
  -> m (Either PlaceOrderError (NonEmpty OrderOptionId))
validateOrderPayloadItems items = do
  checks <- OrderOption.checkExistence items
  pure $ sequence $ validatePair <$> NEL.zip checks items
 where
  validatePair (ooid', iffyOoid) =
    maybe (Left $ UnknownOrderOption iffyOoid) Right ooid'

mkOrder
  :: UUID
  -> [Location]
  -> UserId
  -> UTCTime
  -> NonEmpty Restaurant
  -> NonEmpty OrderOptionId
  -> Either PlaceOrderError Order
mkOrder uuid locations' userId now restaurants ooids =
  case nonEmpty locations' of
    Nothing        -> Left AddressNotFound
    Just locations -> if NEL.length addresses > 1
      then Left $ AmbiguousAddress addresses
      else Right $ Order
        { id           = OrderId uuid
        , status       = Verification
        , payload = OrderPayload { items = ooids, address = NEL.head addresses }
        , restaurantId = (id :: Restaurant -> RestaurantId) closestRestaurant
        , placedAt     = now
        , userId       = userId
        }
     where
      (addresses, coordinates) = NEL.unzip locations
      closestRestaurant =
        getClosestRestaurant restaurants $ NEL.head coordinates


getClosestRestaurant :: NonEmpty Restaurant -> Coordinates -> Restaurant
getClosestRestaurant restaurants coordinates = fst closest where
  closest   = foldr pickClosest x xs
  (x :| xs) = appendDistance coordinates <$> restaurants

type RestaurantDistance = (Restaurant, Distance)

pickClosest :: RestaurantDistance -> RestaurantDistance -> RestaurantDistance
pickClosest r1@(_, d1) r2@(_, d2) = if d1 < d2 then r1 else r2

appendDistance :: Coordinates -> Restaurant -> (Restaurant, Distance)
appendDistance c1 r@Restaurant {..} = (r, getDistance c1 coordinates)
