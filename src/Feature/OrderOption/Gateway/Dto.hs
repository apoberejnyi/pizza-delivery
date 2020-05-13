{-# LANGUAGE OverloadedStrings #-}

module Feature.OrderOption.Gateway.Dto where

import Data.Aeson (Value, object, (.=))
import Feature.OrderOption.Types

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

