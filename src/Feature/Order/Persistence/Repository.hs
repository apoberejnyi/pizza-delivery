{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Feature.Order.Persistence.Repository where

import Base.PG
import Base.Types.Address
import Control.Monad.IO.Class
import Data.List.NonEmpty
import Data.Maybe
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.Types
import Feature.Order.Persistence.Types
import Feature.Order.Types hiding ( DeleteOrder )
import Feature.OrderOption.Types
import Feature.Restaurant.Types

queryAllOrders :: MonadIO m => QueryAllOrders m
queryAllOrders = do
    results <- withConn $ \conn -> query_ conn getQuery
    pure $ fmap unOrderEntity results
        where
    getQuery = "SELECT " <> fields <>  " FROM orders"

queryOrderById :: MonadIO m => QueryOrderById m
queryOrderById (OrderId oid) = do
    results <- withConn $
        \conn -> query conn "SELECT ? FROM orders WHERE id=?" (Only oid)
    pure $ unOrderEntity <$> listToMaybe results

insertOrder :: MonadIO m => InsertOrder m
insertOrder order = do
    _ <- withConn $ \conn -> execute conn insertQuery (OrderEntity order)
    pure ()
        where
    insertQuery = "INSERT INTO orders (" <> fields <> ") VALUES (?, ?, ?::uuid[], ?, ?)"

deleteOrder :: MonadIO m => DeleteOrder m
deleteOrder oid'@(OrderId oid) = do
    updateCount <- withConn $ \conn -> execute conn "DELETE FROM orders WHERE id=?" (Only oid)
    let result = if updateCount == 0
        then Left $ OrderNotFound oid'
        else Right ()
    pure result

fields :: Query
fields = "id, status, items, address, restaurantId"

newtype OrderEntity = OrderEntity { unOrderEntity :: Order }

instance ToRow OrderEntity where
    toRow (OrderEntity Order{..}) =
        let OrderPayload{..} = orderPayload in
        toRow
            ( unOrderId orderId
            , show orderStatus
            , (PGArray . toList) $ unOrderOptionId <$> orderPayloadItems
            , unAddress orderPayloadAddress
            , unRestaurantId orderRestaurantId
            )

instance FromRow OrderEntity where
    fromRow = do
        oid <- field
        status <- field
        PGArray items <- field
        address <- field
        restaurantId <- field

        pure $ OrderEntity $ Order
            { orderId = OrderId oid
            , orderStatus = read status
            , orderPayload = OrderPayload
                { orderPayloadItems = fromList $ OrderOptionId <$> items
                , orderPayloadAddress = Address address
                }
            , orderRestaurantId = RestaurantId restaurantId
            }
