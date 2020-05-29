module TestState where

import Control.Monad.Trans.State
import Data.Function
import Data.List as List
import Data.Maybe

data TestState f = TestState
    { testEvents   :: [TestEvent]
    , testFixtures :: [f]
    }

newtype TestEvent = Called String deriving (Eq, Show)

type Lift f m a = State (TestState f) a -> m a

putEvent :: Monad m => TestEvent -> Lift f m () -> m ()
putEvent event = (&) $ modify (\ts -> ts { testEvents = event : testEvents ts })

getFixtures :: Monad m => Lift f m [f] -> m [f]
getFixtures = (&) $ gets testFixtures

setFixtures :: Monad m => [f] -> Lift f m () -> m ()
setFixtures fs = (&) $ modify (\ts -> ts { testFixtures = fs })

withFixture :: (Monad m, Eq f) => a -> Lift f m a -> ([f] -> [(a , f)]) -> m a
withFixture defaultVal app genVals = app $ gets testFixtures >>= \fixtures ->
    case listToMaybe (genVals fixtures) of
        Nothing     -> pure defaultVal
        Just (v, f) -> do
            modify (\ts -> ts { testFixtures = List.delete f fixtures })
            pure v

