module Feature.User.Contract where

import           Feature.User.Error
import           Feature.User.Types
import           Text.Email.Parser

type Login m = EmailAddress -> Password -> m (Either NotAuthenticated AuthToken)
type RegisterUser m = UserForCreate -> m (Either RegisterUserError User)

class (Monad m) => Service m where
  login :: Login m
  register :: RegisterUser m
