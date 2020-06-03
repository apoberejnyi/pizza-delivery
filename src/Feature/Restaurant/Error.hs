{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Feature.Restaurant.Error where

import           Control.Error
import           Data.Text                     as T
import           Feature.Restaurant.Types

newtype CreateRestaurantError = RestaurantNameAlreadyInUse Text

instance Show CreateRestaurantError where
  show (RestaurantNameAlreadyInUse name') =
    mconcat ["Restaurant ", unpack name', " is already in use"]

instance Error CreateRestaurantError where
  code (RestaurantNameAlreadyInUse _) = "Restaurant_NameAlreadyInUse"

newtype DeleteRestaurantError = RestaurantDidNotExist RestaurantId

instance Show DeleteRestaurantError where
  show (RestaurantDidNotExist rid) =
    mconcat ["Restaurant ", show rid, " not found"]

instance Error DeleteRestaurantError where
  code (RestaurantDidNotExist _) = "Restaurant_NotFound"

newtype GetRestaurantError = RestaurantNotFound RestaurantId

instance Show GetRestaurantError where
  show (RestaurantNotFound rid) =
    mconcat ["Restaurant ", show rid, " not found"]

instance Error GetRestaurantError where
  code (RestaurantNotFound _) = "Restaurant_NotFound"

