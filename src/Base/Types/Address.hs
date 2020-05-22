{-# LANGUAGE DeriveGeneric #-}

module Base.Types.Address where

import Base.Types.Coordinates
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

newtype Address = Address Text deriving (Show, Generic)
instance FromJSON Address
instance ToJSON Address

class Monad m => AddressResolver m where
    resolveAddress :: Address -> m Coordinates
