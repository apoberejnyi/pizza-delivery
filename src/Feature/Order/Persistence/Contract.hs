module Feature.Order.Persistence.Contract where

import Control.Monad.IO.Class
import Feature.Order.Types

class MonadIO m => Repo m where
    queryAll :: m [Order]
    insert :: Order -> m ()
