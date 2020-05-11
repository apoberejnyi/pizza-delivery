{-# LANGUAGE OverloadedStrings #-}
module OrderOption.Persistence (insertOrderOption) where

import Control.Exception (catch, throw)
import Data.Aeson (Value, encode, object, (.=))
import Data.ByteString.Lazy (ByteString)
import Database.PostgreSQL.Simple (Connection, execute, sqlState)
import Database.PostgreSQL.Simple.ToField (Action)
import Database.PostgreSQL.Simple.ToRow (toRow)
import OrderOption
    ( OrderOption (OrderOption)
    , OrderOption
    , OrderOptionId (OrderOptionId)
    , OrderOptionPayload (Pizza)
    , PizzaSize (PizzaSize)
    , RegisterOptionError (NameAlreadyInUse)
    )

insertOrderOption :: Connection -> OrderOption -> IO (Either RegisterOptionError ())
insertOrderOption connection option = catch (Right () <$ result) catchSqlException
      where
    result = execute connection insertQuery (optionToEntity option)
    insertQuery = "INSERT INTO OrderOptions (id, name, sizes) VALUES (?, ?, ?)"
    catchSqlException sqlError
        | sqlState sqlError == "23505" = pure $ Left NameAlreadyInUse
        | otherwise = throw sqlError

optionToEntity :: OrderOption -> [Action]
optionToEntity (OrderOption (OrderOptionId oid) (Pizza name sizes)) =
    toRow (oid, name, sizesToEntity sizes)

sizesToEntity :: [PizzaSize] -> ByteString
sizesToEntity = encode . map sizeToJSON

sizeToJSON :: PizzaSize -> Value
sizeToJSON (PizzaSize diameter cost) = object
        [ "diameter" .= diameter
        , "cost" .= cost
        ]
