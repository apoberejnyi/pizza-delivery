{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RecordWildCards #-}

module Feature.User.Service where

import           Auth.Token                    as Token
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
import           Prelude                 hiding ( id )
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

login :: (Persistence.Repo m, POSIXTimeGen m, Token.Service m) => Login m
login userEmail userPassword = do
  lookupResult <- lookupPwdHash userEmail
  case lookupResult of
    HashNotFound -> notAuthenticated
    HashFound user hash ->
      if validatePassword (encodePassword userPassword) (encodeHash hash)
        then Right <$> (Token.generate user =<< currentTime)
        else notAuthenticated
 where
  notAuthenticated = pure (Left NotAuthenticated)
  encodePassword   = E.encodeUtf8 . unPassword
  encodeHash       = E.encodeUtf8 . unPasswordHash



