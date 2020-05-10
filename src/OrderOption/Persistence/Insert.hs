{-# LANGUAGE OverloadedStrings #-}
module OrderOption.Persistence.Insert (insertOrderOption) where

import Control.Exception (catch, throw)
import Database.PostgreSQL.Simple
    (Connection, Query, SqlError, execute, sqlState)
import OrderOption (OrderOption, RegisterOptionError (NameAlreadyInUse))
import OrderOption.Persistence.Entity (toEntity)

insertOrderOption :: Connection -> OrderOption -> IO (Either RegisterOptionError ())
insertOrderOption connection option = catch (Right () <$ result) catchSqlException
      where
    result = execute connection insertQuery (toEntity option)
    insertQuery = "INSERT INTO OrderOptions (id, name, sizes) VALUES (?, ?, ?)" :: Query

catchSqlException :: SqlError -> IO (Either RegisterOptionError ())
catchSqlException sqlError
    | sqlState sqlError == "23505" = pure $ Left NameAlreadyInUse
    | otherwise = throw sqlError
