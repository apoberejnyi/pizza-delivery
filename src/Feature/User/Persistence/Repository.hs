{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Feature.User.Persistence.Repository where

import           Data.Maybe
import           Text.Email.Parser
import           Feature.User.Error
import           Feature.User.Types
import           Feature.User.Contract
import           Feature.User.Persistence.Contract
import           Feature.User.Persistence.Entity
import           Control.Exception
import           Persistence.PG
import           Database.PostgreSQL.Simple
import           Prelude                 hiding ( id )

lookupUserPwdHash :: (PG r m) => LookupUserPwdHash m
lookupUserPwdHash userEmail = do
  result <- withConn $ \conn -> query conn lookupQuery emailVal
  case listToMaybe result of
    Nothing                     -> pure HashNotFound
    Just (UserEntity user hash) -> pure (HashFound user hash)
 where
  lookupQuery =
    mconcat ["SELECT ", fieldsNames, " FROM ", tableName, " WHERE email=?"]
  emailVal = Only $ toByteString userEmail

insertUser :: PG r m => InsertUser m
insertUser user passwordHash = withConn safeInsert
 where
  safeInsert conn = catch (Right () <$ unsafeInsert conn) catchSqlException
  unsafeInsert conn = execute conn insertQuery entity
  insertQuery = mconcat
    [ "INSERT INTO "
    , tableName
    , " ("
    , fieldsNames
    , ") VALUES ("
    , fieldsPlaceholders
    , ")"
    ]
  entity = UserEntity user passwordHash
  catchSqlException sqlError | sqlState sqlError == "23505" = emailInUse
                             | otherwise                    = throw sqlError
  emailInUse = (pure . Left . EmailAlreadyInUse) (pickEmail user)
  pickEmail  = email . (payload :: User -> UserPayload)



