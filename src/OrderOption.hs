{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module OrderOption
    ( RegisterOrderOption
    , DeleteOrderOption
    , RegisterOptionError(..)
    , GetAllOrderOptions
    , OrderOption(..)
    , OrderOptionId(..)
    , OrderOptionPayload(..)
    , PizzaSize(..)
    ) where

import Data.Aeson
    (FromJSON, ToJSON, Value (Object), object, parseJSON, toJSON, (.:), (.=))
import Data.Text (Text)
import Data.UUID (UUID)
import GHC.Generics (Generic)

type GetAllOrderOptions m = m [OrderOption]
type DeleteOrderOption m = OrderOptionId -> m ()

data RegisterOptionError = NameAlreadyInUse
type RegisterOrderOption m = OrderOptionPayload -> m (Either RegisterOptionError OrderOptionId)

newtype OrderOptionId = OrderOptionId UUID deriving (Eq, Generic)
instance ToJSON OrderOptionId

data OrderOption = OrderOption
    { orderOptionId      :: OrderOptionId
    , orderOptionPayload :: OrderOptionPayload
    }

data OrderOptionPayload = Pizza
    { pizzaName  :: Text
    , pizzaSizes :: [PizzaSize]
    }

instance ToJSON OrderOptionPayload where
    toJSON (Pizza name sizes) = object
        [ "name" .= name
        , "sizes" .= sizes
        ]

instance FromJSON OrderOptionPayload where
    parseJSON (Object v) =
        Pizza <$> v .: "name"
              <*> v .: "sizes"
    parseJSON _ = mempty

newtype PizzaCost = PizzaCost Int deriving (Eq, Generic)
instance ToJSON PizzaCost
instance FromJSON PizzaCost

newtype PizzaDiameter = PizzaDiameter Int deriving (Eq, Generic)
instance ToJSON PizzaDiameter
instance FromJSON PizzaDiameter

data PizzaSize = PizzaSize PizzaDiameter PizzaCost

instance ToJSON PizzaSize where
    toJSON (PizzaSize diameter cost) = object
        [ "diameter" .= diameter
        , "cost" .= cost
        ]

instance FromJSON PizzaSize where
    parseJSON (Object v) =
        PizzaSize
            <$> v .: "diameter"
            <*> v .: "cost"
    parseJSON _ = mempty
