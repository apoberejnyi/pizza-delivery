{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Spec.OrderOption where

import Base.Types.UUID
import Control.Monad.Trans.Writer
import Data.List.NonEmpty
import Data.Maybe
import Data.UUID
import qualified Feature.OrderOption.Persistence.Types
import Feature.OrderOption.Service
import Feature.OrderOption.Types
import Test.Hspec

spec :: Spec
spec =
    describe "Order Options Service" $
        describe "Register Order Option" $
            it "should inject order option into repository" $ do
                let (result , events) = runTest $ registerOrderOption mockPayload
                let expected = Right (OrderOptionId mockUUID)
                result `shouldBe` expected
                events `shouldMatchList` ["inject called"]

mockUUID :: UUID
mockUUID = (fromJust . fromString) "abc370c2-c40f-4515-9468-8e6c2ff64e06"

mockPayload :: OrderOptionPayload
mockPayload = Pizza
    { pizzaName = "Pizza"
    , pizzaSizes = PizzaSize (PizzaDiameter 10) (PizzaCost 20) :| []
    }

runTest :: App a -> (a, [String])
runTest = runWriter . unApp

newtype App m = App { unApp :: Writer [String] m } deriving (Functor, Applicative, Monad)
instance Feature.OrderOption.Persistence.Types.Repo App where
    queryAll = undefined
    queryById = undefined
    filterExisting = undefined
    insert _ = do
        App $ tell ["inject called"]
        pure $ Right ()
    delete = undefined

instance UUIDGen App where
    nextUUID = pure mockUUID

