module Feature.Auth.Config where

import           System.Envy
import           Data.Text                     as T

data JwtConfig = JwtConfig
  { jwtSecret :: T.Text
  , jwtExpirationSeconds :: Int
  }

instance FromEnv JwtConfig where
  fromEnv _ = JwtConfig <$> env "JWT_SECRET" <*> env "JWT_EXPIRATION_SECONDS"
