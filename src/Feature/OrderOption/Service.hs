{-# LANGUAGE OverloadedStrings #-}

module Feature.OrderOption.Service
    ( registerOrderOption
    , getAllOrderOptions
    ) where

import Base.UUID
import Feature.OrderOption.Contract as OO
import Feature.OrderOption.Types

getAllOrderOptions :: (OO.Repo m) => m [OrderOption]
getAllOrderOptions = queryAll

registerOrderOption :: (OO.Repo m, UUIDGen m) =>
    OrderOptionPayload ->  m (Either RegisterOptionError OrderOptionId)
registerOrderOption payload = do
    optionId <- OrderOptionId <$> nextUUID
    result <- insert (OrderOption optionId payload)
    pure (optionId <$ result)


