{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}

module Feature.Auth.Gateway.DTO where

import           Feature.Auth.Types
import           Data.Aeson
import           Data.Text                     as T
import           Gateway.Util
import           GHC.Generics
import           Prelude                 hiding ( id )
import           Feature.User.Gateway.DTO

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

