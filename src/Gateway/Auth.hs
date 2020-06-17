{-# LANGUAGE OverloadedStrings #-}

module Gateway.Auth where

import           Feature.Auth.Contract         as Auth
import           Feature.Auth.Types
import           Feature.User.Contract         as User
import           Feature.User.Error
import           Feature.User.Types
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Web.Scotty.Trans              as ST
import           Network.HTTP.Types
import qualified Data.Text.Lazy                as TL
import           Data.Text.Encoding            as E
import           Network.Wai.Middleware.HttpAuth
import           Prelude

requestUserId
  :: (MonadIO m, ScottyError t, Auth.Service m) => ActionT t m UserId
requestUserId = do
  authHeader <- header "Authorization"
  case extractJwt =<< authHeader of
    Nothing    -> unauthorized
    Just token -> maybe unauthorized pure =<< lift (Auth.validateToken token)
 where
  extractJwt   = fmap toToken . (extractBearerAuth . E.encodeUtf8 . TL.toStrict)
  toToken      = AuthToken . E.decodeUtf8
  unauthorized = status unauthorized401 >> finish

allowRoles
  :: (MonadIO m, ScottyError t, User.Service m, Auth.Service m)
  => [UserRole]
  -> ActionT t m ()
allowRoles allowedRoles = do
  userId <- requestUserId
  user'  <- lift $ User.getById userId
  case user' of
    Left  (UserNotFound _) -> status unauthorized401 >> finish
    Right user             -> do
      let roleAllowed = any (`elem` allowedRoles) (roles user)
      if roleAllowed then pure () else status forbidden403 >> finish
