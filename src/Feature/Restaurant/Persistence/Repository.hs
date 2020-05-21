{-# LANGUAGE OverloadedStrings #-}

module Feature.Restaurant.Persistence.Repository where

import Base.PG
import Base.Types.Coordinates
import Control.Exception
import Control.Monad.IO.Class
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToRow
import Feature.Restaurant.Types

queryAllRestaurants :: MonadIO m => m [Restaurant]
queryAllRestaurants = do
    result <- withConn $ \conn -> query_ conn "SELECT id, name, lat, lon FROM restaurants"
    pure $ fmap unRestaurantEntity result

insertRestaurant :: MonadIO m => Restaurant -> m (Either CreateRestaurantError ())
insertRestaurant restaurant = do
    let result = withConn $ \conn -> execute conn insertQuery (RestaurantEntity restaurant)
    liftIO $ catch (Right () <$ result) catchSqlException
      where
    insertQuery = "INSERT INTO restaurants (id, name, lat, lon) VALUES (?, ?, ?, ?)"
    catchSqlException sqlError
        | sqlState sqlError == "23505" = pure $ Left RestaurantNameAlreadyInUse
        | otherwise = throw sqlError

newtype RestaurantEntity = RestaurantEntity { unRestaurantEntity :: Restaurant }

instance ToRow RestaurantEntity where
    toRow (RestaurantEntity (Restaurant (RestaurantId rid) name (Coordinates lat lon))) =
        toRow (rid, name, lat, lon)

instance FromRow RestaurantEntity where
    fromRow = do
        rid <- field; name <- field; lat <- field; lon <- field
        let restaurant = Restaurant (RestaurantId rid) name (Coordinates lat lon)
        pure $ RestaurantEntity restaurant
