{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Feature.Auth.Service
  ( Feature.Auth.Service.validateToken
  , login
  )
where

import           Feature.User.Types
import           Feature.User.Contract         as User
import           Feature.Auth.Error
import           Feature.Auth.Config
import           Feature.Auth.Contract   hiding ( login )
import           Data.Text.Encoding            as E
import           Feature.Auth.Types
import           Data.UUID                     as UUID
import           Data.Has
import qualified Web.JWT                       as JWT
import           Prelude                 hiding ( id )
import           Data.Time.Clock.POSIX
import           Data.Generate.POSIXTime
import           Control.Monad.Reader
import           Crypto.KDF.BCrypt

login
  :: (Has JwtConfig r, MonadReader r m, User.Service m, POSIXTimeGen m)
  => Login m
login userEmail userPassword = do
  lookupResult <- lookupPwdHash userEmail
  case lookupResult of
    HashNotFound -> notAuthenticated
    HashFound user hash ->
      if validatePassword (encodePassword userPassword) (encodeHash hash)
        then Right <$> (generateToken user =<< currentTime)
        else notAuthenticated
 where
  notAuthenticated = pure (Left NotAuthenticated)
  encodePassword   = E.encodeUtf8 . unPassword
  encodeHash       = E.encodeUtf8 . unPasswordHash

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


