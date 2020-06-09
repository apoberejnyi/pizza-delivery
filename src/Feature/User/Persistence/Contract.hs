module Feature.User.Persistence.Contract where

import           Feature.User.Error
import           Feature.User.Contract
import           Feature.User.Types

type InsertUser m = User -> PasswordHash -> m (Either RegisterUserError ())

class Monad m => Repo m where
  getAll :: GetAllUsers m
  getById :: GetUserById m
  insert :: InsertUser m
  lookupPwdHash :: LookupUserPwdHash m
  delete :: DeleteUser m
