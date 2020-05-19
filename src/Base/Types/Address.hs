{-# LANGUAGE DeriveGeneric #-}

module Base.Types.Address where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

newtype Address = Address Text deriving (Show, Generic)
instance FromJSON Address
instance ToJSON Address
