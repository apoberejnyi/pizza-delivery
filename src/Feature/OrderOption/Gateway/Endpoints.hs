{-# LANGUAGE OverloadedStrings #-}

module Feature.OrderOption.Gateway.Endpoints where

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Feature.OrderOption.Contract as OrderOption
import Feature.OrderOption.Gateway.Dto
import Feature.OrderOption.Types
import Network.HTTP.Types
import Web.Scotty.Trans

endpoints :: (MonadIO m, OrderOption.Service m) => ScottyT LT.Text m ()
endpoints = do
    get "/api/orderOptions" $ do
        result <- lift OrderOption.getAll
        json $ map OrderOptionDto result

    post "/api/orderOptions" $ do
        OrderOptionPayloadDto payload <- jsonData
        result <- lift $ OrderOption.register payload
        case result of
            Right ooid -> json ooid
            Left NameAlreadyInUse -> do
                status conflict409
                json ("Order option name is already in use" :: T.Text)
