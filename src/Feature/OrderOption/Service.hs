{-# LANGUAGE OverloadedStrings #-}

module Feature.OrderOption.Service
  ( registerOrderOption
  , getAllOrderOptions
  , checkOrderOptionsExistence
  , getOrderOptionById
  , deleteOrderOption
  )
where

import           Data.Generate.UUID
import qualified Feature.OrderOption.Persistence.Types
                                               as Persistence
import           Feature.OrderOption.Contract
import           Feature.OrderOption.Error
import           Feature.OrderOption.Types

getAllOrderOptions :: (Persistence.Repo m) => GetAllOrderOptions m
getAllOrderOptions = Persistence.queryAll

getOrderOptionById :: (Persistence.Repo m) => GetOrderOptionById m
getOrderOptionById ooid =
  maybe (Left $ OrderOptionNotFound ooid) Right <$> Persistence.queryById ooid

checkOrderOptionsExistence
  :: (Persistence.Repo m) => CheckOrderOptionExistence m
checkOrderOptionsExistence ids = do
  filtered <- Persistence.filterExisting ids
  pure $ compareWithExisting filtered <$> ids
 where
  compareWithExisting ooids (IffyOrderOptionId val)
    | val `elem` fmap unOrderOptionId ooids = Just $ OrderOptionId val
    | otherwise                             = Nothing

registerOrderOption :: (Persistence.Repo m, UUIDGen m) => RegisterOrderOption m
registerOrderOption optionPayload = do
  optionId <- OrderOptionId <$> nextUUID
  result   <- Persistence.insert (OrderOption optionId optionPayload)
  pure (optionId <$ result)

deleteOrderOption :: (Persistence.Repo m) => DeleteOrderOption m
deleteOrderOption = Persistence.delete

