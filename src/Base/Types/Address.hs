module Base.Types.Address where

import Base.Types.Coordinates
import Data.Text ( Text )

newtype IffyAddress = IffyAddress { unIffyAddress :: Text }
newtype Address = Address { unAddress :: Text } deriving (Eq, Show)

type Location = (Address, Coordinates)

class Monad m => AddressResolver m where
    resolveAddress :: IffyAddress -> m [Location]
