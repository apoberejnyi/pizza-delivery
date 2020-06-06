{-# LANGUAGE OverloadedStrings #-}

module Feature.Order.Persistence.Repository where

import           Control.Monad.IO.Class
import           Data.Maybe
import           Database.PostgreSQL.Simple
import           Feature.Order.Persistence.Types
import           Feature.Order.Persistence.Entity
import           Feature.Order.Error
import           Feature.Order.Types
import           Persistence.PG
import           Prelude                 hiding ( id )

queryAllOrders :: MonadIO m => QueryAllOrders m
queryAllOrders = do
  results <- withConn $ \conn -> query_ conn getQuery
  pure $ fmap unOrderEntity results
  where getQuery = "SELECT " <> fieldNames <> " FROM orders"

queryOrderById :: MonadIO m => QueryOrderById m
queryOrderById (OrderId oid) = do
  results <- withConn $ \conn -> query conn lookupQuery (Only oid)
  pure $ unOrderEntity <$> listToMaybe results
  where lookupQuery = "SELECT " <> fieldNames <> " FROM orders WHERE id=?"

insertOrder :: MonadIO m => InsertOrder m
insertOrder order = do
  _ <- withConn $ \conn -> execute conn insertQuery (OrderEntity order)
  pure ()
 where
  insertQuery = mconcat
    ["INSERT INTO orders (", fieldNames, ") VALUES (", fieldPlaceholders, ")"]

deleteOrder :: MonadIO m => DeleteOrder m
deleteOrder oid'@(OrderId oid) = do
  updateCount <- withConn
    $ \conn -> execute conn "DELETE FROM orders WHERE id=?" (Only oid)
  let result =
        if updateCount == 0 then Left $ OrderDidNotExist oid' else Right ()
  pure result

