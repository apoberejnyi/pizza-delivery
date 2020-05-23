module Base.Types.Address where

import Base.Types.Coordinates
import Data.Text (Text)

newtype IffyAddress = IffyAddress { unIffyAddress :: Text }
newtype Address = Address { unAddress :: Text }

class Monad m => AddressResolver m where
    resolveAddress :: IffyAddress -> m [(Address, Coordinates)]
