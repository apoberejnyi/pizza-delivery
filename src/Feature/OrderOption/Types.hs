{-# LANGUAGE DeriveGeneric #-}

module Feature.OrderOption.Types where

import Data.Aeson
import Data.List.NonEmpty
import Data.Text (Text)
import Data.UUID (UUID)
import GHC.Generics (Generic)

data RegisterOptionError = NameAlreadyInUse
data DeleteOrderOptionError = OrderOptionNotFound OrderOptionId

newtype OrderOptionId = OrderOptionId { unOrderOptionId :: UUID }

data OrderOption = OrderOption
    { orderOptionId      :: OrderOptionId
    , orderOptionPayload :: OrderOptionPayload
    }

data OrderOptionPayload = Pizza
    { pizzaName  :: Text
    , pizzaSizes :: NonEmpty PizzaSize
    }

newtype PizzaCost = PizzaCost Double deriving (Eq, Generic)
instance ToJSON PizzaCost
instance FromJSON PizzaCost

newtype PizzaDiameter = PizzaDiameter Double deriving (Eq, Generic)
instance ToJSON PizzaDiameter
instance FromJSON PizzaDiameter

data PizzaSize = PizzaSize PizzaDiameter PizzaCost
