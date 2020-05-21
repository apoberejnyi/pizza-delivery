{-# LANGUAGE DeriveGeneric #-}

module Feature.OrderOption.Types where

import Data.Aeson
import Data.List.NonEmpty
import Data.Text (Text)
import Data.UUID (UUID)
import GHC.Generics (Generic)

data RegisterOptionError = NameAlreadyInUse
data DeleteOrderOptionError = OrderOptionNotFound OrderOptionId

newtype OrderOptionId = OrderOptionId UUID deriving (Eq, Generic)
instance ToJSON OrderOptionId

data OrderOption = OrderOption
    { orderOptionId      :: OrderOptionId
    , orderOptionPayload :: OrderOptionPayload
    }

data OrderOptionPayload = Pizza
    { pizzaName  :: Text
    , pizzaSizes :: NonEmpty PizzaSize
    }

newtype PizzaCost = PizzaCost Int deriving (Eq, Generic)
instance ToJSON PizzaCost
instance FromJSON PizzaCost

newtype PizzaDiameter = PizzaDiameter Int deriving (Eq, Generic)
instance ToJSON PizzaDiameter
instance FromJSON PizzaDiameter

data PizzaSize = PizzaSize PizzaDiameter PizzaCost
