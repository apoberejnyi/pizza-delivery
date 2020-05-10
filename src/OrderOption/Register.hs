{-# LANGUAGE OverloadedStrings #-}

module OrderOption.Register where

import qualified Data.UUID.V4 as UUID
import Database.PostgreSQL.Simple
import OrderOption

registerOrderOption :: Connection -> RegisterOrderOption IO
registerOrderOption connection payload = do
    optionId <- UUID.nextRandom
    execute
        connection
        "INSERT INTO OrderOptions (id, name, size, price) VALUES (?, ?, ?, ?)"
        (optionId, "Margarita" :: String, 36 :: Int, 12 :: Int)
    pure ()
