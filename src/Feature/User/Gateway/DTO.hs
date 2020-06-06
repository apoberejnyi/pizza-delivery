{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}

module Feature.User.Gateway.DTO where

import           Data.Aeson
import           Data.Address
import           Data.Text                     as T
import           Data.Text.Encoding            as E
import           Data.UUID
import           Feature.User.Types
import           Gateway.Util
import           GHC.Generics
import           Text.Email.Validate           as Email
import           Prelude                 hiding ( id )

instance FromJSON LoginDto
data LoginDto = LoginDto
  { email :: EmailAddressDto
  , password :: Text
  }
  deriving (Generic)

instance ToJSON AuthTokenDto
newtype AuthTokenDto = AuthTokenDto
    { token :: Text
    }
    deriving (Generic)

instance ToDTO AuthTokenDto AuthToken where
  toDTO (AuthToken val) = AuthTokenDto val

instance ToJSON UserDto
data UserDto = UserDto
  { id :: UUID
  , firstName :: Text
  , lastName :: Maybe Text
  , phoneNumber :: Text
  , email :: EmailAddressDto
  , addresses :: [Text]
  }
  deriving (Generic)

instance ToDTO UserDto User where
  toDTO User {..} =
    let UserPayload {..} = payload
    in  UserDto { id          = unUserId id
                , firstName   = firstName
                , lastName    = lastName
                , phoneNumber = phoneNumber
                , email       = EmailAddressDto email
                , addresses   = unAddress <$> addresses
                }

instance FromJSON UserForCreateDto
data UserForCreateDto = UserForCreateDto
  { firstName :: Text
  , lastName :: Maybe Text
  , phoneNumber :: Text
  , email :: EmailAddressDto
  , addresses :: [Text]
  , password :: Text
  }
  deriving (Generic)

instance FromDTO UserForCreateDto UserForCreate where
  fromDTO UserForCreateDto {..} = UserForCreate
    { payload  = UserPayload { firstName   = firstName
                             , lastName    = lastName
                             , phoneNumber = phoneNumber
                             , email       = unEmailAddressDto email
                             , addresses   = Address <$> addresses
                             }
    , password = Password password
    }

newtype EmailAddressDto = EmailAddressDto { unEmailAddressDto :: EmailAddress }
instance FromJSON EmailAddressDto where
  parseJSON = withText "email" $ \val ->
    either fail (pure . EmailAddressDto) $ (Email.validate . E.encodeUtf8) val
instance ToJSON EmailAddressDto where
  toJSON (EmailAddressDto email) =
    (toJSON . E.decodeUtf8 . Email.toByteString) email
