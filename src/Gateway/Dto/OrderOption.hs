{-# LANGUAGE OverloadedStrings #-}

module Gateway.Dto.OrderOption where

import Data.Aeson (Value, object, (.=))
import OrderOption
    ( OrderOption (OrderOption)
    , OrderOptionPayload (Pizza)
    , PizzaSize (PizzaSize)
    )

orderOptionToDto :: OrderOption -> Value
orderOptionToDto (OrderOption ooid (Pizza name sizes)) = object
    [ "id" .= ooid
    , "name" .= name
    , "sizes" .= map sizeToJSON sizes
    ]
  where
    sizeToJSON (PizzaSize diameter cost) = object
        [ "diameter" .= diameter
        , "cost" .= cost
        ]

