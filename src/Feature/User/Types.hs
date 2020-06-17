{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Feature.User.Types where

import           Data.String
import           Data.Text
import           Data.Address
import           Data.UUID
import           Text.Email.Parser

data UserPayload = UserPayload
  { firstName :: Text
  , lastName :: Maybe Text
  , phoneNumber :: Text
  , email :: EmailAddress
  , addresses :: [Address]
  }

data User = User
  { id :: UserId
  , roles :: [UserRole]
  , payload :: UserPayload
  }

data UserForCreate = UserForCreate
  { payload :: UserPayload
  , password :: Password
  }

newtype UserId = UserId { unUserId :: UUID } deriving (Eq)
newtype UserRole = UserRole { unUserRole :: Text } deriving (Eq, IsString)

newtype Password = Password { unPassword :: Text }
newtype PasswordHash = PasswordHash { unPasswordHash :: Text } deriving (Eq)

