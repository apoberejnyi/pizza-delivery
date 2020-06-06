{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Feature.Order.Persistence.Entity where

import           Data.Address
import           Data.List.NonEmpty
import           Data.Maybe
import           Data.Char
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.ToRow
import           Database.PostgreSQL.Simple.Types
import           Feature.Order.Types
import           Feature.OrderOption.Types
import           Feature.Restaurant.Types
import           Feature.User.Types
import           Prelude                 hiding ( id )

fieldNames :: Query
fieldNames =
  "id, status, items, address, restaurant_id, user_id, placed_at, resolved_at"

fieldPlaceholders :: Query
fieldPlaceholders = "?, ?, ?::uuid[], ?, ?, ?, ?, ?"

newtype OrderEntity = OrderEntity { unOrderEntity :: Order }

instance ToRow OrderEntity where
  toRow (OrderEntity Order {..}) =
    let OrderPayload {..} = payload
    in  toRow
          ( unOrderId id
          , showStatus status
          , (PGArray . toList) $ unOrderOptionId <$> items
          , unAddress address
          , unRestaurantId restaurantId
          , unUserId userId
          , placedAt
          , resolvedAtRow
          )
   where
    resolvedAtRow = case status of
      Completed time -> Just time
      _              -> Nothing
    showStatus (Completed _) = "completed"
    showStatus s             = toLower <$> show s



instance FromRow OrderEntity where
  fromRow = do
    oid           <- field
    status        <- field
    PGArray items <- field
    address       <- field
    restaurantId  <- field
    userId        <- field
    placedAt      <- field
    resolvedAt    <- field

    let items' = fromList $ OrderOptionId <$> items
    pure $ OrderEntity $ Order
      { id           = OrderId oid
      , status       = maybe (read status) Completed resolvedAt
      , payload = OrderPayload { items = items', address = Address address }
      , restaurantId = RestaurantId restaurantId
      , placedAt     = placedAt
      , userId       = UserId userId
      }
