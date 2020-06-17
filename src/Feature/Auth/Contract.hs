module Feature.Auth.Contract where

import           Feature.User.Types
import           Feature.Auth.Error
import           Feature.Auth.Types
import           Text.Email.Parser
import           Data.Text

type ValidateToken m = Text -> m (Maybe UserId)
type Login m = EmailAddress -> Password -> m (Either NotAuthenticated AuthToken)

class (Monad m) => Service m where
  validateToken :: ValidateToken m
  login :: Login m
