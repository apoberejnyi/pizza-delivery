module Feature.User.Contract where

import           Feature.User.Error
import           Feature.User.Types
import           Text.Email.Validate

data LookupHashResult = HashFound User PasswordHash | HashNotFound
type LookupUserPwdHash m = EmailAddress -> m LookupHashResult

type RegisterUser m = UserForCreate -> m (Either RegisterUserError User)

class (Monad m) => Service m where
  lookupPwdHash :: LookupUserPwdHash m
  register :: RegisterUser m
