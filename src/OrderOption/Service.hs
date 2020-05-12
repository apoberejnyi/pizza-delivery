{-# LANGUAGE OverloadedStrings #-}

module OrderOption.Service (registerOrderOption) where

import qualified Data.UUID.V4 as UUID
import OrderOption
    ( OrderOption (OrderOption)
    , OrderOptionId (OrderOptionId)
    , RegisterOptionError
    , RegisterOrderOption
    )


type InjectOrderOption = OrderOption -> IO (Either RegisterOptionError ())
registerOrderOption :: InjectOrderOption -> RegisterOrderOption IO
registerOrderOption injectOrderOption payload = do
    optionId <- OrderOptionId <$> UUID.nextRandom
    result <- injectOrderOption (OrderOption optionId payload)
    pure (optionId <$ result)
