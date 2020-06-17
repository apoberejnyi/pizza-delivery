module Feature.Auth.Types where

import           Data.Text

data AuthToken = AuthToken
  { tokenValue :: Text
  , expiresIn :: Int
  }
