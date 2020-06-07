{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Feature.Auth.Service
  ( generateToken
  , validateToken
  )
where

import           Feature.User.Types
import           Feature.Auth.Config
import           Feature.Auth.Types
import           Data.UUID                     as UUID
import           Data.Has
import qualified Web.JWT                       as JWT
import           Prelude                 hiding ( id )
import           Data.Time.Clock.POSIX
import           Control.Monad.Reader

generateToken
  :: (Monad m, Has JwtConfig r, MonadReader r m)
  => User
  -> POSIXTime
  -> m AuthToken
generateToken user now = mkJwt user now <$> asks getter

mkJwt :: User -> POSIXTime -> JwtConfig -> AuthToken
mkJwt user now JwtConfig {..} = AuthToken jwt
 where
  jwt        = JWT.encodeSigned key joseHeader claims
  key        = JWT.hmacSecret jwtSecret
  joseHeader = mempty
  claims     = mempty
    { JWT.sub = JWT.stringOrURI (unpackUserId user)
    , JWT.iat = JWT.numericDate now
    , JWT.exp = JWT.numericDate (now + fromIntegral jwtExpirationSeconds)
    }
  unpackUserId = UUID.toText . unUserId . id

validateToken
  :: (Monad m, Has JwtConfig r, MonadReader r m)
  => AuthToken
  -> m (Maybe UserId)
validateToken token = do
  config <- asks getter
  pure (validateToken' token config)

validateToken' :: AuthToken -> JwtConfig -> Maybe UserId
validateToken' (AuthToken token) JwtConfig {..} =
  parseSub
    =<< JWT.sub
    =<< JWT.claims
    <$> JWT.decodeAndVerifySignature signer token
 where
  signer   = JWT.hmacSecret jwtSecret
  parseSub = fmap UserId . UUID.fromString . show
