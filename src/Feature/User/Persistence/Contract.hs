module Feature.User.Persistence.Contract where

import           Feature.User.Error
import           Feature.User.Types
import           Text.Email.Parser

data LookupHashResult = HashFound User PasswordHash | HashNotFound

type InsertUser m = User -> PasswordHash -> m (Either RegisterUserError ())
type LookupUserPwdHash m = EmailAddress -> m LookupHashResult

class Monad m => Repo m where
  insert :: InsertUser m
  lookupPwdHash :: LookupUserPwdHash m
