{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module OrderOption where

import Data.Aeson
import Data.Text
import Data.UUID
import GHC.Generics

type GetAllOrderOptions m = m [OrderOption]
type RegisterOrderOption m = OrderOptionPayload -> m ()
type DeleteOrderOption m = OrderOptionId -> m ()

newtype OrderOptionId = OrderOptionId UUID

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

newtype PizzaCost = PizzaCost Int deriving (Eq, Generic)
instance ToJSON PizzaCost
instance FromJSON PizzaCost

newtype PizzaDiameter = PizzaDiameter Int deriving (Eq, Generic)
instance ToJSON PizzaDiameter
instance FromJSON PizzaDiameter

type PizzaSize = (PizzaDiameter, PizzaCost)
