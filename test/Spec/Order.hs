{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Spec.Order where

import Base.Concurrency
import Base.Types.Address
import Base.Types.Coordinates
import Base.Types.UUID
import Control.Monad.Trans.State
import Data.List.NonEmpty
import Data.UUID
import Feature.Order.Persistence.Types as Order.Persistence
import Feature.Order.Service
import Feature.Order.Types as Order
import Feature.OrderOption.Types as OrderOption
import Feature.Restaurant.Types as Restaurant
import Test.Hspec
import Test.State
import Test.Util

spec :: Spec
spec = describe "Order Service" $
    describe "Place order" $ do
        it "should choose closest restaurant to order address" $ do
            let payload = IffyOrderPayload
                    { iffyOrderPayloadItems = IffyOrderOptionId (asUUID "88163569-b0f8-4635-85db-d26545ad052a") :| []
                    , iffyOrderPayloadAddress = IffyAddress "Paradise City, Green Grass Street"
                    }
            let resolvedLocations = [(Address "Paradise City, Green Grass Street", Coordinates 10 20)]
            let availableRestaurants =
                    [ Restaurant
                        { restaurantId = RestaurantId (asUUID "20557362-2281-4994-8df9-d7d81395e726")
                        , restaurantName = "Paradise City"
                        , restaurantCoordinates = Coordinates 11 21
                        }
                    , Restaurant
                        { restaurantId = RestaurantId (asUUID "df2cd899-834c-427c-863c-0e9922402243")
                        , restaurantName = "Ivory Tower"
                        , restaurantCoordinates = Coordinates 12 22
                        }
                    ]
            let (result , _) = runTest $
                    setFixtures [ResolveAddress resolvedLocations, GetAllRestaurants availableRestaurants] App >>
                    placeOrder payload
            orderRestaurantId (right result) `shouldBe` RestaurantId (asUUID "20557362-2281-4994-8df9-d7d81395e726")

        it "should insert order upon success" $ do
            let payload = IffyOrderPayload
                    { iffyOrderPayloadItems = IffyOrderOptionId (asUUID "88163569-b0f8-4635-85db-d26545ad052a") :| []
                    , iffyOrderPayloadAddress = IffyAddress "Paradise City, Green Grass Street"
                    }
            let (_ , testState) = runTest $ placeOrder payload
            testEvents testState `shouldContain` [InsertedOrder]

        it "should reject if address is ambiguous" $ do
            let payload = IffyOrderPayload
                    { iffyOrderPayloadItems = IffyOrderOptionId (asUUID "88163569-b0f8-4635-85db-d26545ad052a") :| []
                    , iffyOrderPayloadAddress = IffyAddress "Paradise City, Green Grass Street"
                    }
            let resolvedLocations =
                    [ (Address "Paradise City, Green Grass Street", Coordinates 10 20)
                    , (Address "Paradise City, Blue Sky Street", Coordinates 10 21)
                    ]
            let (result , _) = runTest $
                    setFixtures [ResolveAddress resolvedLocations] App >>
                    placeOrder payload
            left result `shouldBe` AmbiguousAddress (fst <$> fromList resolvedLocations)

        it "should reject if address is not found" $ do
            let payload = IffyOrderPayload
                    { iffyOrderPayloadItems = IffyOrderOptionId (asUUID "88163569-b0f8-4635-85db-d26545ad052a") :| []
                    , iffyOrderPayloadAddress = IffyAddress "Paradise City, Green Grass Street"
                    }
            let (result , _) = runTest $
                    setFixtures [ResolveAddress []] App >>
                    placeOrder payload
            left result `shouldBe` AddressNotFound

        it "should reject if no restaurants are available" $ do
            let payload = IffyOrderPayload
                    { iffyOrderPayloadItems = IffyOrderOptionId (asUUID "88163569-b0f8-4635-85db-d26545ad052a") :| []
                    , iffyOrderPayloadAddress = IffyAddress "Paradise City, Green Grass Street"
                    }
            let (result , _) = runTest $
                    setFixtures [GetAllRestaurants []] App >>
                    placeOrder payload
            left result `shouldBe` NoRestaurantsAvailable

        it "should reject non-existing order options" $ do
            let orderedItems =
                    IffyOrderOptionId (asUUID "88163569-b0f8-4635-85db-d26545ad052a") :|
                    [IffyOrderOptionId (asUUID "631c225f-0a0d-4b04-a937-d87d50005698")]
            let validatedItems =
                    Just (OrderOptionId (asUUID "88163569-b0f8-4635-85db-d26545ad052a")) :|
                    [Nothing]
            let payload = IffyOrderPayload
                    { iffyOrderPayloadItems = orderedItems
                    , iffyOrderPayloadAddress = IffyAddress "Paradise City, Green Grass Street"
                    }
            let (result , _) = runTest $
                    setFixtures [CheckOrderOptionsExistence validatedItems] App >>
                    placeOrder payload
            left result `shouldBe` UnknownOrderOption (IffyOrderOptionId (asUUID "631c225f-0a0d-4b04-a937-d87d50005698"))

data Fixtures = GenerateUUID UUID
    | GetAllRestaurants [Restaurant]
    | CheckOrderOptionsExistence (NonEmpty (Maybe OrderOptionId))
    | ResolveAddress [Location]
    deriving (Eq)

data Events = InsertedOrder
    deriving (Eq, Show)

type OrderTest = TestState Events Fixtures
newtype App m = App { unApp :: State OrderTest m } deriving (Functor, Applicative, Monad)

instance AddressResolver App where
    resolveAddress _ = withFixture defaultLocations App $ \fs -> do
        f@(ResolveAddress v) <- fs
        pure (v,f)
            where
        defaultLocations = [(Address "Paradise City, Green Grass Street", Coordinates 10 20)]

instance Order.Persistence.Repo App where
    insert _ = do
        putEvent InsertedOrder App
        pure ()

instance Restaurant.Service App where
    getAll = withFixture [defaultRestaurant] App $ \fs -> do
        f@(GetAllRestaurants v) <- fs
        pure (v,f)
            where
        defaultRestaurant = Restaurant
            { restaurantId = RestaurantId (asUUID "20557362-2281-4994-8df9-d7d81395e726")
            , restaurantName = "Paradise City"
            , restaurantCoordinates = Coordinates 11 21
            }

instance OrderOption.Service App where
    checkExistence ids = withFixture defaultVal App $ \fs -> do
        f@(CheckOrderOptionsExistence v) <- fs
        pure (v,f)
            where
        defaultVal = fmap markValid ids
        markValid = Just . OrderOptionId . unIffyOrderOptionId

instance Concurrent App where
    concurrently3 a b c = do
        a' <- a; b' <- b; c' <- c
        pure (a', b', c')

instance UUIDGen App where
    nextUUID = withFixture (error "No default UUID") App $ \fs -> do
        f@(GenerateUUID v) <- fs
        pure (v,f)

runTest :: App a -> (a, OrderTest)
runTest app = runState (unApp app) (TestState [] [])
