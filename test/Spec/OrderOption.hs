{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Spec.OrderOption where

import Base.Types.UUID
import Control.Monad.Trans.State
import Data.List.NonEmpty
import Data.Maybe
import Data.UUID
import qualified Feature.OrderOption.Persistence.Types
import Feature.OrderOption.Service
import Feature.OrderOption.Types
import Test.Hspec
import TestState

spec :: Spec
spec =
    describe "Order Options Service" $ do
        describe "Register Order Option" $
            it "should inject order option into repository" $ do
                let uuid = asUUID "abc370c2-c40f-4515-9468-8e6c2ff64e06"
                let expected = Right (OrderOptionId uuid)
                let payload = Pizza
                        { pizzaName = "Pizza"
                        , pizzaSizes = PizzaSize (PizzaDiameter 10) (PizzaCost 20) :| []
                        }

                let (result , TestState{..}) = runTest $
                        setFixtures [GenerateUUID uuid] App >>
                        registerOrderOption payload

                result `shouldBe` expected
                testEvents `shouldMatchList` [Called "inject"]

        describe "Check order options existence" $
            it "should check order options existence" $ do
                let ids = fromList $ fmap asUUID ["abc370c2-c40f-4515-9468-8e6c2ff64e06"]
                let idsToCheck = fmap IffyOrderOptionId ids
                let filteredIds = toList $ fmap OrderOptionId ids
                let expected = fmap (Just . OrderOptionId) ids

                let (result, _) = runTest $
                        setFixtures [CheckExistence filteredIds] App >>
                        checkOrderOptionsExistence idsToCheck

                result `shouldBe` expected

newtype App m = App { unApp :: State OrderOptionTest m } deriving (Functor, Applicative, Monad)

instance Feature.OrderOption.Persistence.Types.Repo App where
    queryAll = undefined
    queryById = undefined
    filterExisting _ = do
        putEvent (Called "filterExisting") App
        withFixture [] App $ \fs -> do
            f@(CheckExistence v) <- fs
            pure (v,f)
    insert _ = do
        putEvent (Called "inject") App
        pure $ Right ()
    delete = undefined

instance UUIDGen App where
    nextUUID = withFixture (error "No default UUID") App $ \fs -> do
        f@(GenerateUUID v) <- fs
        pure (v,f)

data Fixture = GenerateUUID UUID
    | CheckExistence [OrderOptionId]
    | GetById (Maybe OrderOption)
    deriving (Eq)

type OrderOptionTest = TestState Fixture

asUUID :: String -> UUID
asUUID = fromJust . fromString

runTest :: App a -> (a, OrderOptionTest)
runTest app = runState (unApp app) (TestState [] [])
