{-# LANGUAGE OverloadedStrings #-}

module Feature.User.Error where

import           Feature.User.Types
import           Control.Error
import           Text.Email.Parser

newtype UserNotFound = UserNotFound UserId

instance Show UserNotFound where
  show (UserNotFound userId) = mconcat ["User '", show userId, "' not found"]

instance Error UserNotFound where
  code (UserNotFound _) = "User_NotFound"

newtype RegisterUserError = EmailAlreadyInUse EmailAddress deriving (Eq)

instance Show RegisterUserError where
  show (EmailAlreadyInUse userEmail) =
    mconcat ["Email ", show userEmail, " is already in use"]

instance Error RegisterUserError where
  code (EmailAlreadyInUse _) = "User_EmailAlreadyInUse"
