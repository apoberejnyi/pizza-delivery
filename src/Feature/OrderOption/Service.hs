{-# LANGUAGE OverloadedStrings #-}

module Feature.OrderOption.Service
    ( registerOrderOption
    , getAllOrderOptions
    , checkOrderOptionsExistence
    , getOrderOptionById
    , deleteOrderOption
    ) where

import Base.Types.UUID
import Data.List.NonEmpty
import qualified Feature.OrderOption.Persistence.Types as Persistence
import Feature.OrderOption.Types

getAllOrderOptions :: (Persistence.Repo m) => m [OrderOption]
getAllOrderOptions = Persistence.queryAll

getOrderOptionById :: (Persistence.Repo m) => OrderOptionId -> m (Maybe OrderOption)
getOrderOptionById = Persistence.queryById

checkOrderOptionsExistence :: (Persistence.Repo m) => NonEmpty IffyOrderOptionId -> m (NonEmpty (Maybe OrderOptionId))
checkOrderOptionsExistence ids = do
    filtered <- Persistence.filterExisting ids
    pure $ compareWithExisting filtered <$> ids
        where
    compareWithExisting ooids (IffyOrderOptionId val)
        | val `elem` fmap unOrderOptionId ooids = Just $ OrderOptionId val
        | otherwise = Nothing

registerOrderOption :: (Persistence.Repo m, UUIDGen m) =>
    OrderOptionPayload ->  m (Either RegisterOptionError OrderOptionId)
registerOrderOption payload = do
    optionId <- OrderOptionId <$> nextUUID
    result <- Persistence.insert (OrderOption optionId payload)
    pure (optionId <$ result)

deleteOrderOption :: (Persistence.Repo m) => OrderOptionId -> m (Either DeleteOrderOptionError ())
deleteOrderOption = Persistence.delete

