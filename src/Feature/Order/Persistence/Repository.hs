{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Feature.Order.Persistence.Repository where

import Base.PG
import Base.Types.Address
import Control.Monad.IO.Class
import Data.List.NonEmpty
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.Types
import Feature.Order.Types
import Feature.Order.Persistence.Types
import Feature.OrderOption.Types
import Feature.Restaurant.Types

queryAllOrders :: MonadIO m => QueryAllOrders m
queryAllOrders = do
    results <- withConn $ \conn -> query_ conn "SELECT id, items, address, restaurantId FROM orders"
    pure $ fmap unOrderEntity results

insertOrder :: MonadIO m => InsertOrder m
insertOrder order = do
    _ <- withConn $ \conn -> execute conn insertQuery (OrderEntity order)
    pure ()
        where
    insertQuery = "INSERT INTO orders (id, items, address, restaurantId) VALUES (?, ?::uuid[], ?, ?)"

newtype OrderEntity = OrderEntity { unOrderEntity :: Order }

instance ToRow OrderEntity where
    toRow (OrderEntity Order{..}) =
        let OrderPayload{..} = orderPayload in
        toRow
            ( unOrderId orderId
            , (PGArray . toList) $ unOrderOptionId <$> orderPayloadItems
            , unAddress orderPayloadAddress
            , unRestaurantId orderRestaurantId
            )

instance FromRow OrderEntity where
    fromRow = do
        oid <- field; PGArray items <- field; address <- field; restaurantId <- field
        pure $ OrderEntity $ Order
            { orderId = OrderId oid
            , orderPayload = OrderPayload
                { orderPayloadItems = fromList $ OrderOptionId <$> items
                , orderPayloadAddress = Address address
                }
            , orderRestaurantId = RestaurantId restaurantId
            }
