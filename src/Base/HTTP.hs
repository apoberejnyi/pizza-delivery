{-# LANGUAGE OverloadedStrings #-}

module Base.HTTP where

import Control.Monad.IO.Class
import Data.Aeson
import qualified Data.Text.Lazy as LT
import Data.UUID
import Network.HTTP.Types
import Web.Scotty.Trans as ST

parseBody :: (FromJSON a, MonadIO m, ScottyError t) => ActionT t m a
parseBody = do
    rawBody <- body
    let parsed = eitherDecode rawBody
    case parsed of
        Right a -> pure a
        Left message -> do
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
