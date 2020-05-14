module Base.HTTP where

import Control.Monad.IO.Class
import Data.Aeson
import Network.HTTP.Types
import Web.Scotty.Trans as ST

parseBody :: (FromJSON a, MonadIO m, ScottyError t) => ActionT t m a
parseBody = do
    rawBody <- body
    let parsed = eitherDecode rawBody
    case parsed of
        Right a -> pure a
        Left message -> do
            status badRequest400
            ST.json message
            finish
