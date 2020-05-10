{-# LANGUAGE OverloadedStrings #-}
module OrderOption.Persistence.Entity (toEntity) where

import Data.Aeson (Value, encode, object, (.=))
import Data.ByteString.Lazy (ByteString)
import Database.PostgreSQL.Simple.ToField (Action)
import Database.PostgreSQL.Simple.ToRow (toRow)
import OrderOption
    ( OrderOption (OrderOption)
    , OrderOptionId (OrderOptionId)
    , OrderOptionPayload (Pizza)
    , PizzaSize (PizzaSize)
    )

toEntity :: OrderOption -> [Action]
toEntity (OrderOption (OrderOptionId oid) (Pizza name sizes)) =
    toRow (oid, name, sizesToEntity sizes)

sizesToEntity :: [PizzaSize] -> ByteString
sizesToEntity sizes = encode $ map sizeToJSON sizes

sizeToJSON :: PizzaSize -> Value
sizeToJSON (PizzaSize diameter cost) = object
        [ "diameter" .= diameter
        , "cost" .= cost
        ]
