{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Spec.OrderOption where

import           Control.Monad.Trans.State
import           Data.List.NonEmpty
import           Data.Maybe
import           Data.UUID
import qualified Feature.OrderOption.Persistence.Types
import           Feature.OrderOption.Service
import           Feature.OrderOption.Types
import           Data.Generate.UUID
import           Test.Hspec
import           Test.State

spec :: Spec
spec = describe "Order Options Service" $ do
  describe "Register Order Option"
    $ it "should inject order option into repository"
    $ do
        let uuid     = u "abc370c2-c40f-4515-9468-8e6c2ff64e06"
        let expected = Right (OrderOptionId uuid)
        let optionPayload = Pizza
              { name  = "Pizza"
              , sizes = PizzaSize (PizzaDiameter 10) (PizzaCost 20) :| []
              }
        let (result, testState) =
              runTest
                $  (Test . setFixtures) [GenerateUUID uuid]
                >> registerOrderOption optionPayload
        result `shouldBe` expected
        testEvents testState `shouldContain` [CalledInject]

  describe "Check order options existence"
    $ it "should check order options existence"
    $ do
        let idsToCheck = fromList
              [ IffyOrderOptionId (u "86a5996f-a474-426a-bf00-3de6d316ab1f")
              , IffyOrderOptionId (u "c215e053-f7a8-43b7-9647-d34fbd0d5b00")
              , IffyOrderOptionId (u "24d85d9e-ae85-485e-8999-e572b0734e65")
              ]
        let filteredIds =
              [ OrderOptionId (u "86a5996f-a474-426a-bf00-3de6d316ab1f")
              , OrderOptionId (u "24d85d9e-ae85-485e-8999-e572b0734e65")
              ]
        let expected = fromList
              [ Just $ OrderOptionId (u "86a5996f-a474-426a-bf00-3de6d316ab1f")
              , Nothing
              , Just $ OrderOptionId (u "24d85d9e-ae85-485e-8999-e572b0734e65")
              ]

        let (result, _) =
              runTest
                $  (Test . setFixtures) [CheckExistence filteredIds]
                >> checkOrderOptionsExistence idsToCheck

        result `shouldBe` expected

newtype Test m = Test { unTest :: State OrderOptionTest m } deriving (Functor, Applicative, Monad)

instance Feature.OrderOption.Persistence.Types.Repo Test where
  filterExisting _ = do
    (Test . putEvent) CalledFilterExisting
    (Test . withFixture []) $ \fs -> do
      f@(CheckExistence v) <- fs
      pure (v, f)
  insert _ = do
    Test $ putEvent CalledInject
    pure $ Right ()

instance UUIDGen Test where
  nextUUID = Test $ withFixture (error "No default UUID") $ \fs -> do
    f@(GenerateUUID v) <- fs
    pure (v, f)

data TestEvent = CalledInject
    | CalledFilterExisting
    deriving (Eq, Show)

data Fixture = GenerateUUID UUID
    | CheckExistence [OrderOptionId]
    | GetById (Maybe OrderOption)
    deriving (Eq)

type OrderOptionTest = TestState TestEvent Fixture

u :: String -> UUID
u = fromJust . fromString

runTest :: Test a -> (a, OrderOptionTest)
runTest test = runState (unTest test) (TestState [] [])
