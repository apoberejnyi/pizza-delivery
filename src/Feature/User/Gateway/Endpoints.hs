{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Feature.User.Gateway.Endpoints where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import qualified Data.Text.Lazy                as LT
import           Feature.User.Gateway.DTO
import qualified Feature.User.Contract         as User
import           Gateway.Error
import           Gateway.Util
import           Feature.User.Error
import           Feature.User.Types
import           Network.HTTP.Types
import           Web.Scotty.Trans              as S

endpoints :: (MonadIO m, User.Service m) => ScottyT LT.Text m ()
endpoints = do
  get "/api/users" $ do
    result <- lift User.getAll
    json (fmap toDTO result :: [UserDto])

  get "/api/users/:userId" $ do
    userId <- uuidParam "userId"
    result <- lift $ User.getById (UserId userId)
    case result of
      Left  err@(UserNotFound _) -> httpError notFound404 err
      Right user                 -> json (toDTO user :: UserDto)

  post "/api/users" $ do
    (createDto :: UserForCreateDto) <- parseBody
    result                          <- lift $ User.register (fromDTO createDto)
    case result of
      Left err@(EmailAlreadyInUse _) -> httpError conflict409 err
      Right user -> status created201 >> json (toDTO user :: UserDto)

  delete "/api/users/:userId" $ do
    userId <- uuidParam "userId"
    result <- lift $ User.delete (UserId userId)
    case result of
      Left  err@(UserNotFound _) -> httpError notFound404 err
      Right _                    -> finish

