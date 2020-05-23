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
import Feature.OrderOption.Persistence.Contract as OrderOption
import Feature.OrderOption.Types

getAllOrderOptions :: (Repo m) => m [OrderOption]
getAllOrderOptions = queryAll

getOrderOptionById :: (Repo m) => OrderOptionId -> m (Maybe OrderOption)
getOrderOptionById = queryById

checkOrderOptionsExistence :: (Repo m) => NonEmpty IffyOrderOptionId -> m (NonEmpty (Maybe OrderOptionId))
checkOrderOptionsExistence ids = do
    filtered <- filterExisting ids
    pure $ checkExistence filtered <$> ids
        where
    checkExistence ooids (IffyOrderOptionId val)
        | val `elem` fmap unOrderOptionId ooids = Just $ OrderOptionId val
        | otherwise = Nothing

registerOrderOption :: (Repo m, UUIDGen m) =>
    OrderOptionPayload ->  m (Either RegisterOptionError OrderOptionId)
registerOrderOption payload = do
    optionId <- OrderOptionId <$> nextUUID
    result <- OrderOption.insert (OrderOption optionId payload)
    pure (optionId <$ result)

deleteOrderOption :: (Repo m) => OrderOptionId -> m (Either DeleteOrderOptionError ())
deleteOrderOption = delete

