module Base.UUID where

import Data.UUID

class Monad m => UUIDGen m where
    nextUUID :: m UUID
