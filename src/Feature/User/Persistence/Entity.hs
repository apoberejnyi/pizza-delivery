{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Feature.User.Persistence.Entity where

import           Data.Address
import           Text.Email.Validate
import           Data.Aeson
import           Data.List
import           Data.Maybe
import           Feature.User.Types
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.ToRow
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.Types
import           Prelude                 hiding ( id )

tableName :: Query
tableName = "users"

fields :: [Query]
fields =
  [ "id"
  , "first_name"
  , "last_name"
  , "email"
  , "phone_number"
  , "addresses"
  , "pwd_hash"
  , "roles"
  ]

fieldsNames :: Query
fieldsNames = mconcat $ intersperse ", " fields

fieldsPlaceholders :: Query
fieldsPlaceholders = "?, ?, ?, ?, ?, ?::json[], ?, ?"

data UserEntity = UserEntity User PasswordHash

fromEntity :: UserEntity -> User
fromEntity (UserEntity u _) = u

instance ToRow UserEntity where
  toRow (UserEntity User {..} (PasswordHash hash)) = toRow
    ( uuid
    , firstName
    , lastName
    , toByteString email
    , phoneNumber
    , toAddressArray addresses
    , hash
    , PGArray (unUserRole <$> roles)
    )
   where
    UserId uuid      = id
    UserPayload {..} = payload
    toAddressArray   = PGArray . fmap (String . unAddress)

instance FromRow UserEntity where
  fromRow = do
    uuid               <- field
    firstName          <- field
    lastName           <- field
    email'             <- field
    phoneNumber        <- field
    PGArray addresses' <- field
    passwordHash'      <- field
    PGArray roles      <- field

    let user = User
          { id      = UserId uuid
          , roles   = UserRole <$> roles
          , payload = UserPayload { firstName   = firstName
                                  , lastName    = lastName
                                  , phoneNumber = phoneNumber
                                  , email = (fromJust . emailAddress) email'
                                  , addresses   = parseAddress <$> addresses'
                                  }
          }
    pure $ UserEntity user (PasswordHash passwordHash')
   where
    parseAddress val = case fromJSON val of
      Error   _ -> error "Invalid user address DTO"
      Success a -> Address a
