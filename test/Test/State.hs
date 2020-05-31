module Test.State where

import Control.Monad.Trans.State
import Data.List as List
import Data.Maybe

data TestState e f = TestState
    { testEvents   :: [e]
    , testFixtures :: [f]
    }

putEvent :: e -> State (TestState e f) ()
putEvent event = modify (\ts -> ts { testEvents = event : testEvents ts })

getFixtures :: State (TestState e f) [f]
getFixtures = gets testFixtures

setFixtures :: [f] -> State (TestState e f) ()
setFixtures fs = modify (\ts -> ts { testFixtures = fs })

type UnwrapFixture f a = [f] -> [(a , f)]

withFixture :: (Eq f) => a -> UnwrapFixture f a -> State (TestState e f) a
withFixture defaultVal unwrap = getFixtures >>= \fixtures ->
    case listToMaybe (unwrap fixtures) of
        Nothing     -> pure defaultVal
        Just (v, f) -> do
            modify (\ts -> ts { testFixtures = List.delete f fixtures })
            pure v

