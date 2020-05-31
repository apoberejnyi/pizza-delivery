module Data.Address where

import Data.Coordinates
import Data.Text ( Text )

newtype IffyAddress = IffyAddress { unIffyAddress :: Text }
newtype Address = Address { unAddress :: Text } deriving (Eq, Show)

type Location = (Address, Coordinates)

class Monad m => AddressResolver m where
    resolveAddress :: IffyAddress -> m [Location]
