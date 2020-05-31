{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Feature.OrderOption.Types where

import Control.Error
import Data.Aeson
import Data.List.NonEmpty
import Data.Text
import Data.UUID ( UUID )
import GHC.Generics ( Generic )

type GetAllOrderOptions m = m [OrderOption]
type GetOrderOptionById m = OrderOptionId -> m (Either GetOrderOptionError OrderOption)
type CheckOrderOptionExistence m = NonEmpty IffyOrderOptionId -> m (NonEmpty (Maybe OrderOptionId))
type RegisterOrderOption m = OrderOptionPayload -> m (Either RegisterOptionError OrderOptionId)
type DeleteOrderOption m = OrderOptionId -> m (Either DeleteOrderOptionError ())

class Monad m => Service m where
    getAll :: GetAllOrderOptions m
    getById :: GetOrderOptionById m
    checkExistence :: CheckOrderOptionExistence m
    register :: RegisterOrderOption m
    delete :: DeleteOrderOption m

newtype IffyOrderOptionId = IffyOrderOptionId { unIffyOrderOptionId :: UUID } deriving (Eq, Show)
newtype OrderOptionId = OrderOptionId { unOrderOptionId :: UUID } deriving (Eq, Show)

data OrderOption = OrderOption
    { orderOptionId      :: OrderOptionId
    , orderOptionPayload :: OrderOptionPayload
    }
    deriving (Eq)

data OrderOptionPayload = Pizza
    { pizzaName  :: Text
    , pizzaSizes :: NonEmpty PizzaSize
    }
    deriving (Eq)

data OrderOptionExistence = Exist
    | DoesNotExist

newtype PizzaCost = PizzaCost Double deriving (Eq, Generic)
instance ToJSON PizzaCost
instance FromJSON PizzaCost

newtype PizzaDiameter = PizzaDiameter Double deriving (Eq, Generic)
instance ToJSON PizzaDiameter
instance FromJSON PizzaDiameter

data PizzaSize = PizzaSize PizzaDiameter PizzaCost
    deriving (Eq)

newtype GetOrderOptionError = OrderOptionNotFound OrderOptionId
instance Show GetOrderOptionError where
    show (OrderOptionNotFound ooid) = mconcat ["Order option ", show ooid, " not found"]
instance Error GetOrderOptionError where
    code (OrderOptionNotFound _) = "OrderOption_NotFound"

newtype RegisterOptionError = NameAlreadyInUse Text deriving (Eq)
instance Show RegisterOptionError where
    show (NameAlreadyInUse name) = mconcat ["Order option name ", unpack name, " is already in use"]
instance Error RegisterOptionError where
    code (NameAlreadyInUse _) = "OrderOption_NameAlreadyInUse"

newtype DeleteOrderOptionError = OrderOptionDidNotExist OrderOptionId deriving (Eq)
instance Show DeleteOrderOptionError where
    show (OrderOptionDidNotExist ooid) = mconcat ["Order option ", show ooid, " not found"]
instance Error DeleteOrderOptionError where
    code (OrderOptionDidNotExist _) = "OrderOption_NotFound"
