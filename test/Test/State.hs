module Test.State where

import Control.Monad.Trans.State
import Data.Function
import Data.List as List
import Data.Maybe

data TestState e f = TestState
    { testEvents   :: [e]
    , testFixtures :: [f]
    }

type Lift e f m a = State (TestState e f) a -> m a

putEvent :: Monad m => e -> Lift e f m () -> m ()
putEvent event = (&) $ modify (\ts -> ts { testEvents = event : testEvents ts })

getFixtures :: Monad m => Lift e f m [f] -> m [f]
getFixtures = (&) $ gets testFixtures

setFixtures :: Monad m => [f] -> Lift e f m () -> m ()
setFixtures fs = (&) $ modify (\ts -> ts { testFixtures = fs })

type UnwrapFixture f a = [f] -> [(a , f)]

withFixture :: (Monad m, Eq f) => a -> Lift e f m a -> UnwrapFixture f a -> m a
withFixture defaultVal app unwrap = app $ gets testFixtures >>= \fixtures ->
    case listToMaybe (unwrap fixtures) of
        Nothing     -> pure defaultVal
        Just (v, f) -> do
            modify (\ts -> ts { testFixtures = List.delete f fixtures })
            pure v

