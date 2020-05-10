module Base.ResolveAddress where

import Base.Address (Address)
import Base.Coordinates (Coordinates)

type ResolveAddress m = Address -> m Coordinates
