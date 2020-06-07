{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RecordWildCards #-}

module Feature.User.Service where

import           Feature.User.Contract
import           Feature.User.Types
import           Feature.User.Persistence.Contract
                                               as Persistence
import           Crypto.Random.Types
import           Crypto.KDF.BCrypt
import           Data.Text.Encoding            as E
import           Prelude                 hiding ( id )
import           Data.Generate.UUID

lookupUserPasswordHash :: (Persistence.Repo m) => LookupUserPwdHash m
lookupUserPasswordHash = Persistence.lookupPwdHash

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



