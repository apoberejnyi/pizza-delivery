{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Gateway.Util where

import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.ByteString
import           Data.Text.Encoding
import qualified Data.Text.Lazy                as LT
import           Data.UUID
import           Network.HTTP.Types
import           Network.Wai
import           Web.Scotty.Trans              as ST

class FromDTO d m where
    fromDTO :: d -> m

class ToDTO d m where
    toDTO :: m -> d

parseBody :: (FromJSON a, MonadIO m, ScottyError t) => ActionT t m a
parseBody = do
  rawBody <- body
  let parsed = eitherDecode rawBody
  case parsed of
    Right a       -> pure a
    Left  message -> do
      status badRequest400
      ST.json message
      finish

uuidParam :: (MonadIO m, ScottyError t) => LT.Text -> ActionT t m UUID
uuidParam paramName = do
  rawParam <- param paramName
  case fromString rawParam of
    Nothing -> do
      status badRequest400
      ST.json $ mconcat ["Expected UUID for parameter ", paramName]
      finish
    Just uuid -> pure uuid

notFoundRoute :: (MonadIO m, ScottyError t) => ScottyT t m ()
notFoundRoute = notFound $ do
  r <- request
  let m       = requestMethod r
  let path    = rawPathInfo r
  let message = mconcat ["Cannot ", m, " ", path] :: ByteString
  status notFound404
  ST.json $ decodeUtf8 message
