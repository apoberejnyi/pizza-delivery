{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
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
data AuthTokenDto = AuthTokenDto
    { access_token :: Text
    , token_type :: Text
    , expires_in :: Int
    }
    deriving (Generic)

instance ToDTO AuthTokenDto AuthToken where
  toDTO token = AuthTokenDto { access_token = tokenValue token
                             , token_type   = "bearer"
                             , expires_in   = expiresIn token
                             }

