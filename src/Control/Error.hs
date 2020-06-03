module Control.Error where

import           Data.Aeson
import           Data.Text

class (Show e) => Error e where
   code :: e -> Text
   details :: e -> Maybe Value
   details _ = Nothing
