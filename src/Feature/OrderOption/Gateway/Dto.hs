{-# LANGUAGE OverloadedStrings #-}

module Feature.OrderOption.Gateway.Dto where

import Data.Aeson
import Data.Aeson.Extra.Merge
import Feature.OrderOption.Types

newtype OrderOptionDto = OrderOptionDto OrderOption
instance ToJSON OrderOptionDto where
    toJSON (OrderOptionDto (OrderOption ooid payload)) =
        object ["id" .= ooid] `lodashMerge` toJSON (OrderOptionPayloadDto payload)

newtype OrderOptionPayloadDto = OrderOptionPayloadDto OrderOptionPayload
instance FromJSON OrderOptionPayloadDto where
    parseJSON jsonVal = do
        payload <- parseJSON jsonVal
        pure $ OrderOptionPayloadDto payload
instance ToJSON OrderOptionPayloadDto where
    toJSON (OrderOptionPayloadDto (Pizza name sizes)) = object
        [ "name" .= name
        , "sizes" .= map sizeToJSON sizes
        ]
      where
        sizeToJSON (PizzaSize diameter cost) = object
            [ "diameter" .= diameter
            , "cost" .= cost
            ]

