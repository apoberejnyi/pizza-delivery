{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Feature.Order.Persistence.Repository where

import           Control.Monad.IO.Class
import           Data.Address
import           Data.List.NonEmpty
import           Data.Maybe
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.ToRow
import           Database.PostgreSQL.Simple.Types
import           Feature.Order.Persistence.Types
import           Feature.Order.Error
import           Feature.Order.Types
import           Feature.OrderOption.Types
import           Feature.Restaurant.Types
import           Persistence.PG
import           Prelude                 hiding ( id )

queryAllOrders :: MonadIO m => QueryAllOrders m
queryAllOrders = do
  results <- withConn $ \conn -> query_ conn getQuery
  pure $ fmap unOrderEntity results
  where getQuery = "SELECT " <> fields <> " FROM orders"

queryOrderById :: MonadIO m => QueryOrderById m
queryOrderById (OrderId oid) = do
  results <- withConn $ \conn -> query conn lookupQuery (Only oid)
  pure $ unOrderEntity <$> listToMaybe results
  where lookupQuery = "SELECT " <> fields <> " FROM orders WHERE id=?"

insertOrder :: MonadIO m => InsertOrder m
insertOrder order = do
  _ <- withConn $ \conn -> execute conn insertQuery (OrderEntity order)
  pure ()
 where
  insertQuery =
    "INSERT INTO orders (" <> fields <> ") VALUES (?, ?, ?::uuid[], ?, ?)"

deleteOrder :: MonadIO m => DeleteOrder m
deleteOrder oid'@(OrderId oid) = do
  updateCount <- withConn
    $ \conn -> execute conn "DELETE FROM orders WHERE id=?" (Only oid)
  let result =
        if updateCount == 0 then Left $ OrderDidNotExist oid' else Right ()
  pure result

fields :: Query
fields = "id, status, items, address, restaurant_id"

newtype OrderEntity = OrderEntity { unOrderEntity :: Order }

instance ToRow OrderEntity where
  toRow (OrderEntity Order {..}) =
    let OrderPayload {..} = payload
    in  toRow
          ( unOrderId id
          , show status
          , (PGArray . toList) $ unOrderOptionId <$> items
          , unAddress address
          , unRestaurantId restaurantId
          )

instance FromRow OrderEntity where
  fromRow = do
    oid           <- field
    status        <- field
    PGArray items <- field
    address       <- field
    restaurantId  <- field

    let items' = fromList $ OrderOptionId <$> items
    pure $ OrderEntity $ Order
      { id           = OrderId oid
      , status       = read status
      , payload = OrderPayload { items = items', address = Address address }
      , restaurantId = RestaurantId restaurantId
      }
