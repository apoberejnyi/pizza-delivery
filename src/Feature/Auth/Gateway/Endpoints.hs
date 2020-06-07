{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Feature.Auth.Gateway.Endpoints where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import qualified Data.Text.Lazy                as LT
import           Feature.Auth.Gateway.DTO
import           Feature.User.Gateway.DTO
import           Feature.Auth.Error
import           Feature.Auth.Contract         as Auth
import           Feature.User.Types
import           Gateway.Util
import           Network.HTTP.Types
import           Web.Scotty.Trans              as S

endpoints :: (MonadIO m, Auth.Service m) => ScottyT LT.Text m ()
endpoints = post "/api/login" $ do
  LoginDto {..} <- parseBody
  result <- lift $ Auth.login (unEmailAddressDto email) (Password password)
  case result of
    Left  NotAuthenticated -> status unauthorized401 >> finish
    Right authToken        -> json (toDTO authToken :: AuthTokenDto)
