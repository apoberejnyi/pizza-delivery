{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Auth.Token
  ( generateToken
  , validateToken
  , AuthToken(..)
  , Service
  , generate
  , validate
  , JwtConfig
  )
where

import           Feature.User.Types
import           Data.UUID                     as UUID
import           Data.Has
import qualified Web.JWT                       as JWT
import           Prelude                 hiding ( id )
import           Data.Time.Clock.POSIX
import           System.Envy
import           Data.Text                     as T
import           Control.Monad.Trans.Reader

class (Monad m) => Service m where
  generate :: User -> POSIXTime -> m AuthToken
  validate :: AuthToken -> m (Maybe UserId)

generateToken
  :: (Monad m, Has JwtConfig r) => User -> POSIXTime -> ReaderT r m AuthToken
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
  :: (Monad m, Has JwtConfig r) => AuthToken -> ReaderT r m (Maybe UserId)
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

newtype AuthToken = AuthToken Text

data JwtConfig = JwtConfig
  { jwtSecret :: T.Text
  , jwtExpirationSeconds :: Int
  }

instance FromEnv JwtConfig where
  fromEnv _ = JwtConfig <$> env "JWT_SECRET" <*> env "JWT_EXPIRATION_SECONDS"
