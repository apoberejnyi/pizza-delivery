{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Feature.OrderOption.Gateway.Endpoints where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import qualified Data.Text.Lazy                as LT
import           Feature.OrderOption.Gateway.DTO
import qualified Feature.OrderOption.Contract  as OrderOption
import           Feature.OrderOption.Error
import           Feature.OrderOption.Types
import           Gateway.Error
import           Gateway.Util
import           Network.HTTP.Types
import           Web.Scotty.Trans              as S

endpoints :: (MonadIO m, OrderOption.Service m) => ScottyT LT.Text m ()
endpoints = do
  get "/api/orderOptions" $ do
    result <- lift OrderOption.getAll
    json (map toDTO result :: [OrderOptionDto])

  get "/api/orderOptions/:id" $ do
    ooid   <- uuidParam "id"
    result <- lift $ OrderOption.getById (OrderOptionId ooid)
    case result of
      Left  err@(OrderOptionNotFound _) -> httpError notFound404 err
      Right oo                          -> json (toDTO oo :: OrderOptionDto)

  post "/api/orderOptions" $ do
    (payloadDto :: OrderOptionPayloadDto) <- parseBody
    result <- lift $ OrderOption.register (fromDTO payloadDto)
    case result of
      Left err@(NameAlreadyInUse _) -> httpError conflict409 err
      Right ooid -> status created201 >> json (unOrderOptionId ooid)

  delete "/api/orderOptions/:id" $ do
    ooid   <- uuidParam "id"
    result <- lift $ OrderOption.delete (OrderOptionId ooid)
    case result of
      Left  err@(OrderOptionDidNotExist _) -> httpError notFound404 err
      Right _                              -> finish
