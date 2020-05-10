{-# LANGUAGE OverloadedStrings #-}

module OrderOption.Register where

import Control.Exception
import Data.Aeson
import qualified Data.UUID.V4 as UUID
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToRow
import OrderOption

data RegisterOptionError = NameAlreadyInUse
type RegisterOrderOption m = OrderOptionPayload -> m (Either RegisterOptionError OrderOptionId)


registerOrderOption :: Connection -> RegisterOrderOption IO
registerOrderOption connection payload = do
    optionId <- OrderOptionId <$> UUID.nextRandom
    let option = OrderOption optionId payload
    let insert = execute connection insertQuery (asRow option)
    catch (Right optionId <$ insert) catchSqlException
      where
    insertQuery = "INSERT INTO OrderOptions (id, name, sizes) VALUES (?, ?, ?)"
    asRow (OrderOption (OrderOptionId oid) (Pizza name sizes)) = toRow (oid, name, encode sizes)


catchSqlException :: SqlError -> IO (Either RegisterOptionError OrderOptionId)
catchSqlException sqlError
    | sqlState sqlError == "23505" = pure $ Left NameAlreadyInUse
    | otherwise = throw sqlError
