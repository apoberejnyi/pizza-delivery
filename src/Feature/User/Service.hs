{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RecordWildCards #-}

module Feature.User.Service where

import           Feature.User.Config
import           Feature.User.Contract
import           Feature.User.Error
import           Feature.User.Types
import           Feature.User.Persistence.Contract
                                               as Persistence
import           Crypto.KDF.BCrypt              ( hashPassword
                                                , validatePassword
                                                )
import           Crypto.Random.Types
import           Data.Text.Encoding            as E
import qualified Web.JWT                       as JWT
import           Prelude                 hiding ( id )
import qualified Data.UUID                     as UUID
import           Data.Time.Clock.POSIX
import           Data.Generate.POSIXTime
import           Data.Generate.UUID

registerUser :: (Persistence.Repo m, UUIDGen m, MonadRandom m) => RegisterUser m
registerUser UserForCreate {..} = do
  userId <- UserId <$> nextUUID
  hash   <- decodeHash <$> hashPassword hashCost (encodePassword password)
  let user = User { id = userId, payload = payload }
  result <- insert user hash
  pure (user <$ result)
 where
  hashCost       = 12
  encodePassword = E.encodeUtf8 . unPassword
  decodeHash     = PasswordHash . E.decodeUtf8

login :: (Persistence.Repo m, POSIXTimeGen m) => JwtConfig -> Login m
login jwtConfig userEmail userPassword = do
  lookupResult <- lookupPwdHash userEmail
  case lookupResult of
    HashNotFound -> notAuthenticated
    HashFound user hash ->
      if validatePassword (encodePassword userPassword) (encodeHash hash)
        then Right . generateJwt jwtConfig user <$> currentTime
        else notAuthenticated
 where
  notAuthenticated = pure (Left NotAuthenticated)
  encodePassword   = E.encodeUtf8 . unPassword
  encodeHash       = E.encodeUtf8 . unPasswordHash

generateJwt :: JwtConfig -> User -> POSIXTime -> AuthToken
generateJwt JwtConfig {..} user now = AuthToken jwt
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

