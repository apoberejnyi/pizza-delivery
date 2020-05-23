{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Feature.OrderOption.Gateway.Endpoints where

import Base.HTTP
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
        json (map fromModel result :: [OrderOptionDto])

    get "/api/orderOptions/:id" $ do
        ooid <- uuidParam "id"
        result <- lift $ OrderOption.getById (OrderOptionId ooid)
        case result of
            Nothing -> do
                status notFound404
                json $ mconcat ["Order option ", show ooid, " not found"]
            Just oo -> json (fromModel oo :: OrderOptionDto)

    post "/api/orderOptions" $ do
        (payload :: OrderOptionPayloadDto) <- parseBody
        result <- lift $ OrderOption.register (toModel payload)
        case result of
            Right ooid -> do
                status created201
                json ooid
            Left NameAlreadyInUse -> do
                status conflict409
                json ("Order option name is already in use" :: T.Text)

    delete "/api/orderOptions/:id" $ do
        ooid <- uuidParam "id"
        result <- lift $ OrderOption.delete (OrderOptionId ooid)
        case result of
            Left (OrderOptionNotFound _) -> do
                status notFound404
                json $ mconcat ["Order option ", show ooid, " not found"]
            Right _ -> finish
