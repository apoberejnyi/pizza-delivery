{-# LANGUAGE DeriveGeneric #-}

module Feature.OrderOption.Types where

import           Data.Aeson
import           Data.List.NonEmpty
import           Data.Text
import           Data.UUID                      ( UUID )
import           GHC.Generics                   ( Generic )

newtype IffyOrderOptionId = IffyOrderOptionId { unIffyOrderOptionId :: UUID } deriving (Eq, Show)
newtype OrderOptionId = OrderOptionId { unOrderOptionId :: UUID } deriving (Eq, Show)

data OrderOption = OrderOption
    { id      :: OrderOptionId
    , payload :: OrderOptionPayload
    }
    deriving (Eq)

data OrderOptionPayload = Pizza
    { name  :: Text
    , sizes :: NonEmpty PizzaSize
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
