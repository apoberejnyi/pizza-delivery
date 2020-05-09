module Base.ResolveAddress where

import           Base.Domain (Address, Coordinates)

type ResolveAddress m = Address -> m Coordinates
