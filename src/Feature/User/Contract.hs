module Feature.User.Contract where

import           Feature.User.Error
import           Feature.User.Types
import           Text.Email.Validate

data LookupHashResult = HashFound User PasswordHash | HashNotFound

type LookupUserPwdHash m = EmailAddress -> m LookupHashResult
type RegisterUser m = UserForCreate -> m (Either RegisterUserError User)
type GetAllUsers m = m [User]
type GetUserById m = UserId -> m (Either UserNotFound User)
type DeleteUser m = UserId -> m (Either UserNotFound ())

class (Monad m) => Service m where
  getAll :: GetAllUsers m
  getById :: GetUserById m
  lookupPwdHash :: LookupUserPwdHash m
  register :: RegisterUser m
  delete :: DeleteUser m
