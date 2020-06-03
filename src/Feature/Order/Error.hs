{-# LANGUAGE OverloadedStrings #-}

module Feature.Order.Error where

import           Control.Error
import           Data.Address
import           Data.Aeson
import           Data.List.NonEmpty
import           Data.Text
import           Feature.Order.Types
import           Feature.OrderOption.Types

newtype GetOrderError = OrderNotFound OrderId deriving (Eq)

instance Show GetOrderError where
    show (OrderNotFound oid) =
        mconcat ["Order ", show $ unOrderId oid, " not found"]

instance Error GetOrderError where
    code (OrderNotFound _) = "Order_NotFound"

data PlaceOrderError = NoRestaurantsAvailable
    | AddressNotFound
    | AmbiguousAddress (NonEmpty Address)
    | UnknownOrderOption IffyOrderOptionId
    deriving (Eq)

instance Show PlaceOrderError where
    show NoRestaurantsAvailable =
        "No restaurant is available to process the order"
    show AddressNotFound      = "Unable to identify order address"
    show (AmbiguousAddress _) = "Provided address is ambiguous"
    show (UnknownOrderOption ooid) =
        mconcat ["Unknown order option ", show $ unIffyOrderOptionId ooid]

instance Error PlaceOrderError where
    code NoRestaurantsAvailable = "Order_NoRestaurantAvailable"
    code AddressNotFound        = "Order_AddressNotFound"
    code (AmbiguousAddress   _) = "Order_AmbiguousAddress"
    code (UnknownOrderOption _) = "Order_UnknownOrderOption"
    details (AmbiguousAddress addresses) =
        Just $ object ["variants" .= (unAddress <$> addresses :: NonEmpty Text)]
    details _ = Nothing

newtype DeleteOrderError = OrderDidNotExist OrderId deriving (Eq)

instance Show DeleteOrderError where
    show (OrderDidNotExist oid) =
        mconcat ["Order ", show $ unOrderId oid, " not found"]

instance Error DeleteOrderError where
    code (OrderDidNotExist _) = "Order_NotFound"
