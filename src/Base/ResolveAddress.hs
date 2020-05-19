module Base.ResolveAddress where

import Base.Types.Address (Address)
import Base.Types.Coordinates (Coordinates)

type ResolveAddress m = Address -> m Coordinates
