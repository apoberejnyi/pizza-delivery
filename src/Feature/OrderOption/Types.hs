{-# LANGUAGE DeriveGeneric #-}

module Feature.OrderOption.Types where

import Data.Aeson
import Data.List.NonEmpty
import Data.Text ( Text )
import Data.UUID ( UUID )
import GHC.Generics ( Generic )

class Monad m => Service m where
    getAll :: m [OrderOption]
    getById :: OrderOptionId -> m (Maybe OrderOption)
    checkExistence :: NonEmpty IffyOrderOptionId -> m (NonEmpty (Maybe OrderOptionId))
    register :: OrderOptionPayload -> m (Either RegisterOptionError OrderOptionId)
    delete :: OrderOptionId -> m (Either DeleteOrderOptionError ())

data RegisterOptionError = NameAlreadyInUse
    deriving (Eq, Show)
newtype DeleteOrderOptionError = OrderOptionNotFound OrderOptionId

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
