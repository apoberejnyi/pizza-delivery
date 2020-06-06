{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Feature.User.Persistence.Repository where

import           Data.Maybe
import           Text.Email.Parser
import           Feature.User.Error
import           Feature.User.Types
import           Feature.User.Persistence.Contract
import           Feature.User.Persistence.Entity
import           Control.Monad.IO.Class
import           Control.Exception
import           Persistence.PG
import           Database.PostgreSQL.Simple
import           Prelude                 hiding ( id )

lookupUserPwdHash :: (MonadIO m) => LookupUserPwdHash m
lookupUserPwdHash userEmail = do
  result <- withConn $ \conn -> query conn lookupQuery emailVal
  case listToMaybe result of
    Nothing                     -> pure HashNotFound
    Just (UserEntity user hash) -> pure (HashFound user hash)
 where
  lookupQuery =
    mconcat ["SELECT ", fieldsNames, " FROM ", tableName, " WHERE email=?"]
  emailVal = Only $ toByteString userEmail

insertUser :: (MonadIO m) => InsertUser m
insertUser user passwordHash = do
  let result = withConn $ \conn -> execute conn insertQuery entity
  liftIO $ catch (Right () <$ result) catchSqlException
 where
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



