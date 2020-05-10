{-# LANGUAGE DeriveGeneric #-}

module Base.Address where

import Data.Aeson
import Data.Text
import GHC.Generics

newtype Address = Address Text deriving (Show, Generic)
instance FromJSON Address
instance ToJSON Address
