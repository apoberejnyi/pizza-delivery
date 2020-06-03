{-# LANGUAGE OverloadedStrings #-}

module Feature.OrderOption.Error where

import           Control.Error
import           Data.Text
import           Feature.OrderOption.Types

newtype GetOrderOptionError = OrderOptionNotFound OrderOptionId

instance Show GetOrderOptionError where
  show (OrderOptionNotFound ooid) =
    mconcat ["Order option ", show ooid, " not found"]

instance Error GetOrderOptionError where
  code (OrderOptionNotFound _) = "OrderOption_NotFound"

newtype RegisterOptionError = NameAlreadyInUse Text deriving (Eq)

instance Show RegisterOptionError where
  show (NameAlreadyInUse name') =
    mconcat ["Order option name ", unpack name', " is already in use"]

instance Error RegisterOptionError where
  code (NameAlreadyInUse _) = "OrderOption_NameAlreadyInUse"

newtype DeleteOrderOptionError = OrderOptionDidNotExist OrderOptionId deriving (Eq)

instance Show DeleteOrderOptionError where
  show (OrderOptionDidNotExist ooid) =
    mconcat ["Order option ", show ooid, " not found"]

instance Error DeleteOrderOptionError where
  code (OrderOptionDidNotExist _) = "OrderOption_NotFound"
