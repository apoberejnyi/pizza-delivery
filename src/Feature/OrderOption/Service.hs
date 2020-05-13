{-# LANGUAGE OverloadedStrings #-}

module Feature.OrderOption.Service (registerOrderOption) where

import Base.UUID
import Feature.OrderOption.Contract as OO
import Feature.OrderOption.Types

registerOrderOption :: (OO.Repo m, UUIDGen m) =>
    OrderOptionPayload ->  m (Either RegisterOptionError OrderOptionId)
registerOrderOption payload = do
    optionId <- OrderOptionId <$> nextUUID
    result <- insert (OrderOption optionId payload)
    pure (optionId <$ result)


