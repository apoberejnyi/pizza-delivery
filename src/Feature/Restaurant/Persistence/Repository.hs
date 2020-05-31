{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Feature.Restaurant.Persistence.Repository
    ( queryAllRestaurants
    , queryRestaurantById
    , insertRestaurant
    , deleteRestaurant
    ) where

import Control.Exception
import Control.Monad.IO.Class
import Data.Coordinates
import Data.Maybe
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToRow
import Feature.Restaurant.Persistence.Types as Persistence
import Feature.Restaurant.Types
import Persistence.PG

queryAllRestaurants :: MonadIO m => QueryAllRestaurants m
queryAllRestaurants = do
    result <- withConn $ \conn -> query_ conn "SELECT id, name, lat, lon FROM restaurants"
    pure $ fmap unRestaurantEntity result

queryRestaurantById :: MonadIO m => QueryRestaurantById m
queryRestaurantById (RestaurantId rid) = do
    result <- withConn $ \conn ->
        query conn "SELECT id, name, lat, lon FROM restaurants WHERE id=? LIMIT 1" (Only rid)
    pure $ unRestaurantEntity <$> listToMaybe result

insertRestaurant :: MonadIO m => InsertRestaurant m
insertRestaurant restaurant@Restaurant{..} = do
    let result = withConn $ \conn -> execute conn insertQuery (RestaurantEntity restaurant)
    liftIO $ catch (Right () <$ result) catchSqlException
      where
    insertQuery = "INSERT INTO restaurants (id, name, lat, lon) VALUES (?, ?, ?, ?)"
    catchSqlException sqlError
        | sqlState sqlError == "23505" = (pure . Left . RestaurantNameAlreadyInUse) restaurantName
        | otherwise = throw sqlError

deleteRestaurant :: MonadIO m => Persistence.DeleteRestaurant m
deleteRestaurant rid'@(RestaurantId rid) = do
    updateCount <- withConn $ \conn -> execute conn "DELETE FROM restaurants WHERE id=?" (Only rid)
    let result = if updateCount == 0
        then Left $ RestaurantDidNotExist rid'
        else Right ()
    pure result

newtype RestaurantEntity = RestaurantEntity { unRestaurantEntity :: Restaurant }

instance ToRow RestaurantEntity where
    toRow (RestaurantEntity (Restaurant (RestaurantId rid) name (Coordinates lat lon))) =
        toRow (rid, name, lat, lon)

instance FromRow RestaurantEntity where
    fromRow = do
        rid <- field; name <- field; lat <- field; lon <- field
        let restaurant = Restaurant (RestaurantId rid) name (Coordinates lat lon)
        pure $ RestaurantEntity restaurant
