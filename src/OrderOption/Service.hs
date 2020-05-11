{-# LANGUAGE OverloadedStrings #-}

module OrderOption.Service (registerOrderOption, getAllOrderOptions) where

import qualified Data.UUID.V4 as UUID
import OrderOption
    ( GetAllOrderOptions
    , OrderOption (OrderOption)
    , OrderOptionId (OrderOptionId)
    , RegisterOptionError
    , RegisterOrderOption
    )


getAllOrderOptions :: Monad m => GetAllOrderOptions m
getAllOrderOptions = undefined

type InjectOrderOption = OrderOption -> IO (Either RegisterOptionError ())
registerOrderOption :: InjectOrderOption -> RegisterOrderOption IO
registerOrderOption injectOrderOption payload = do
    optionId <- OrderOptionId <$> UUID.nextRandom
    result <- injectOrderOption (OrderOption optionId payload)
    pure (optionId <$ result)
