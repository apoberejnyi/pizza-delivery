{-# LANGUAGE OverloadedStrings #-}

module Feature.User.Error where

import           Control.Error
import           Text.Email.Parser

data NotAuthenticated = NotAuthenticated

newtype RegisterUserError = EmailAlreadyInUse EmailAddress deriving (Eq)

instance Show RegisterUserError where
  show (EmailAlreadyInUse email) =
    mconcat ["Email ", show email, " is already in use"]

instance Error RegisterUserError where
  code (EmailAlreadyInUse _) = "User_EmailAlreadyInUse"
