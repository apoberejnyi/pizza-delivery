{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Feature.User.Gateway.Endpoints where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import qualified Data.Text.Lazy                as LT
import           Feature.User.Gateway.DTO
import           Feature.User.Contract         as User
import           Feature.User.Error
import           Gateway.Error
import           Gateway.Util
import           Network.HTTP.Types
import           Web.Scotty.Trans              as S

endpoints :: (MonadIO m, User.Service m) => ScottyT LT.Text m ()
endpoints = post "/api/users" $ do
  (createDto :: UserForCreateDto) <- parseBody
  result                          <- lift $ User.register (fromDTO createDto)
  case result of
    Left err@(EmailAlreadyInUse _) -> httpError conflict409 err
    Right user -> status created201 >> json (toDTO user :: UserDto)
