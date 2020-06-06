{-# LANGUAGE OverloadedStrings #-}

module Gateway.Auth where

import           Auth.Token                    as Token
import           Feature.User.Types
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Web.Scotty.Trans              as ST
import           Network.HTTP.Types
import           Data.Text.Lazy                as TL
import           Data.Text.Encoding            as E
import           Network.Wai.Middleware.HttpAuth
import           Prelude

requestUserId
  :: (MonadIO m, ScottyError t, Token.Service m) => ActionT t m UserId
requestUserId = do
  authHeader <- header "Authorization"
  case extractJwt =<< authHeader of
    Nothing    -> unauthorized
    Just token -> maybe unauthorized pure =<< lift (Token.validate token)
 where
  extractJwt   = fmap toToken . (extractBearerAuth . E.encodeUtf8 . TL.toStrict)
  toToken      = AuthToken . E.decodeUtf8
  unauthorized = status unauthorized401 >> finish


