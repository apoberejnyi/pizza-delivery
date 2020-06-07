module Feature.Auth.Contract where

import           Data.Time.Clock.POSIX
import           Feature.Auth.Types
import           Feature.User.Types

type GenerateToken m = User -> POSIXTime -> m AuthToken
type ValidateToken m = AuthToken -> m (Maybe UserId)

class (Monad m) => Service m where
  generate :: GenerateToken m
  validate :: ValidateToken m
