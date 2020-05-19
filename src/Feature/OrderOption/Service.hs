{-# LANGUAGE OverloadedStrings #-}

module Feature.OrderOption.Service
    ( registerOrderOption
    , getAllOrderOptions
    , getOrderOptionById
    , deleteOrderOption
    ) where

import Base.Types.UUID
import Feature.OrderOption.Persistence.Contract
import Feature.OrderOption.Types

getAllOrderOptions :: (Repo m) => m [OrderOption]
getAllOrderOptions = queryAll

getOrderOptionById :: (Repo m) => OrderOptionId -> m (Maybe OrderOption)
getOrderOptionById = queryById

registerOrderOption :: (Repo m, UUIDGen m) =>
    OrderOptionPayload ->  m (Either RegisterOptionError OrderOptionId)
registerOrderOption payload = do
    optionId <- OrderOptionId <$> nextUUID
    result <- insert (OrderOption optionId payload)
    pure (optionId <$ result)

deleteOrderOption :: (Repo m) => OrderOptionId -> m (Either DeleteOrderOptionError ())
deleteOrderOption = delete

