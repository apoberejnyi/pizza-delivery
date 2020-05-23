module Base.Types.Address where

import Base.Types.Coordinates
import Data.Text (Text)

newtype Address = Address { unAddress :: Text }

data ResolveAddressError = NoCoordinatesFound
    | AmbiguousCoordinates [Address]

class Monad m => AddressResolver m where
    resolveAddress :: Address -> m (Either ResolveAddressError Coordinates)
