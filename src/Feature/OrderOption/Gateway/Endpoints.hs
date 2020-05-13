{-# LANGUAGE OverloadedStrings #-}

module Feature.OrderOption.Gateway.Endpoints where

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import qualified Data.Text.Lazy as LT
import qualified Feature.OrderOption.Contract as OrderOption
import Feature.OrderOption.Gateway.Dto
import Web.Scotty.Trans

endpoints :: (MonadIO m, OrderOption.Service m) => ScottyT LT.Text m ()
endpoints =
    get "/api/orderOptions" $ do
        result <- lift OrderOption.getAll
        json $ map orderOptionToDto result
