{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Feature.Restaurant.Persistence.Repository
  ( queryAllRestaurants
  , queryRestaurantById
  , insertRestaurant
  , deleteRestaurant
  )
where

import           Control.Exception
import           Data.Coordinates
import           Data.Maybe
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.ToRow
import           Feature.Restaurant.Error
import           Feature.Restaurant.Types
import           Feature.Restaurant.Persistence.Types
import           Persistence.PG
import           Prelude                 hiding ( id )

queryAllRestaurants :: (PG r m) => QueryAllRestaurants m
queryAllRestaurants = do
  result <- withConn
    $ \conn -> query_ conn "SELECT id, name, lat, lon FROM restaurants"
  pure $ fmap unRestaurantEntity result

queryRestaurantById :: (PG r m) => QueryRestaurantById m
queryRestaurantById (RestaurantId rid) = do
  result <- withConn $ \conn -> query
    conn
    "SELECT id, name, lat, lon FROM restaurants WHERE id=? LIMIT 1"
    (Only rid)
  pure $ unRestaurantEntity <$> listToMaybe result

insertRestaurant :: (PG r m) => InsertRestaurant m
insertRestaurant restaurant@Restaurant {..} = withConn safeInsert
 where
  safeInsert conn = catch (Right () <$ unsafeInsert conn) catchSqlException
  unsafeInsert conn = execute conn insertQuery (RestaurantEntity restaurant)
  insertQuery =
    "INSERT INTO restaurants (id, name, lat, lon) VALUES (?, ?, ?, ?)"
  catchSqlException sqlError | sqlState sqlError == "23505" = nameInUse
                             | otherwise                    = throw sqlError
  nameInUse = (pure . Left . RestaurantNameAlreadyInUse) name

deleteRestaurant :: PG r m => DeleteRestaurant m
deleteRestaurant rid'@(RestaurantId rid) = do
  updateCount <- withConn
    $ \conn -> execute conn "DELETE FROM restaurants WHERE id=?" (Only rid)
  let result = if updateCount == 0
        then Left $ RestaurantDidNotExist rid'
        else Right ()
  pure result

newtype RestaurantEntity = RestaurantEntity { unRestaurantEntity :: Restaurant }

instance ToRow RestaurantEntity where
  toRow (RestaurantEntity (Restaurant (RestaurantId rid) name (Coordinates lat lon)))
    = toRow (rid, name, lat, lon)

instance FromRow RestaurantEntity where
  fromRow = do
    rid  <- field
    name <- field
    lat  <- field
    lon  <- field
    let restaurant = Restaurant (RestaurantId rid) name (Coordinates lat lon)
    pure $ RestaurantEntity restaurant
