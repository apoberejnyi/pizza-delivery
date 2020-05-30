module Test.Util where

import Data.Maybe
import Data.UUID

asUUID :: String -> UUID
asUUID = fromJust . fromString

right :: Either a b -> b
right (Right v) = v
right (Left _)  = error "Right value expected"

left :: Either a b -> a
left (Right _) = error "Left value expected"
left (Left v)  = v
