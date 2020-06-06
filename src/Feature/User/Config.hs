module Feature.User.Config where

import           Data.Text                     as T
import           System.Envy

data JwtConfig = JwtConfig
  { jwtSecret :: T.Text
  , jwtExpirationSeconds :: Int
  }

instance FromEnv JwtConfig where
  fromEnv _ = JwtConfig <$> env "JWT_SECRET" <*> env "JWT_EXPIRATION_SECONDS"
