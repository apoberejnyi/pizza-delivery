{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

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
import Test.State

spec :: Spec
spec = describe "Order Options Service" $ do
    describe "Register Order Option" $
        it "should inject order option into repository" $ do
            let uuid = asUUID "abc370c2-c40f-4515-9468-8e6c2ff64e06"
            let expected = Right (OrderOptionId uuid)
            let payload = Pizza
                    { pizzaName = "Pizza"
                    , pizzaSizes = PizzaSize (PizzaDiameter 10) (PizzaCost 20) :| []
                    }

            let (result , testState) = runTest $
                    setFixtures [GenerateUUID uuid] App >>
                    registerOrderOption payload

            result `shouldBe` expected
            testEvents testState `shouldContain` [CalledInject]

    describe "Check order options existence" $
        it "should check order options existence" $ do
            let idsToCheck = fromList
                    [ IffyOrderOptionId (asUUID "86a5996f-a474-426a-bf00-3de6d316ab1f")
                    , IffyOrderOptionId (asUUID "c215e053-f7a8-43b7-9647-d34fbd0d5b00")
                    , IffyOrderOptionId (asUUID "24d85d9e-ae85-485e-8999-e572b0734e65")
                    ]
            let filteredIds =
                    [ OrderOptionId (asUUID "86a5996f-a474-426a-bf00-3de6d316ab1f")
                    , OrderOptionId (asUUID "24d85d9e-ae85-485e-8999-e572b0734e65")
                    ]
            let expected = fromList
                    [ Just $ OrderOptionId (asUUID "86a5996f-a474-426a-bf00-3de6d316ab1f")
                    , Nothing
                    , Just $ OrderOptionId (asUUID "24d85d9e-ae85-485e-8999-e572b0734e65")
                    ]

            let (result, _) = runTest $
                    setFixtures [CheckExistence filteredIds] App >>
                    checkOrderOptionsExistence idsToCheck

            result `shouldBe` expected

newtype App m = App { unApp :: State OrderOptionTest m } deriving (Functor, Applicative, Monad)

instance Feature.OrderOption.Persistence.Types.Repo App where
    filterExisting _ = do
        putEvent CalledFilterExisting App
        withFixture [] App $ \fs -> do
            f@(CheckExistence v) <- fs
            pure (v,f)
    insert _ = do
        putEvent CalledInject App
        pure $ Right ()

instance UUIDGen App where
    nextUUID = withFixture (error "No default UUID") App $ \fs -> do
        f@(GenerateUUID v) <- fs
        pure (v,f)

data TestEvent = CalledInject
    | CalledFilterExisting
    deriving (Eq, Show)

data Fixture = GenerateUUID UUID
    | CheckExistence [OrderOptionId]
    | GetById (Maybe OrderOption)
    deriving (Eq)

type OrderOptionTest = TestState TestEvent Fixture

asUUID :: String -> UUID
asUUID = fromJust . fromString

runTest :: App a -> (a, OrderOptionTest)
runTest app = runState (unApp app) (TestState [] [])
