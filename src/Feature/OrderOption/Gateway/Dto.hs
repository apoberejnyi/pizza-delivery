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
    parseJSON = withObject "OrderOptionPayloadDto" $ \v -> do
        name <- v .: "name"
        sizes <- v .: "sizes"
        pure $ OrderOptionPayloadDto $ Pizza name (map unPizzaSizeDto sizes)

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

newtype PizzaSizeDto = PizzaSizeDto { unPizzaSizeDto :: PizzaSize }

instance FromJSON PizzaSizeDto where
    parseJSON = withObject "PizzaSizeDto" $ \v -> do
        size <- PizzaSize
            <$> v .: "diameter"
            <*> v .: "cost"
        pure $ PizzaSizeDto size

instance ToJSON PizzaSizeDto where
    toJSON (PizzaSizeDto (PizzaSize diameter cost)) = object
        [ "diameter" .= diameter
        , "cost" .= cost
        ]
