{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Spec.Order where

import           Control.Concurrency
import           Control.Monad.Trans.State
import           Data.Address
import           Data.Coordinates
import           Data.List.NonEmpty
import           Data.UUID
import           Feature.Order.Persistence.Types
                                               as Order.Persistence
import           Feature.Order.Error
import           Feature.Order.Service
import           Feature.Order.Types           as Order
import           Feature.OrderOption.Contract  as OrderOption
import           Feature.OrderOption.Types
import           Feature.Restaurant.Contract   as Restaurant
import           Feature.Restaurant.Types
import           Persistence.UUID
import           Test.Hspec
import           Test.State
import           Test.Util

spec :: Spec
spec = describe "Order Service" $ describe "Place order" $ do
  it "should choose closest restaurant to order address" $ do
    let orderItems = fromList
          [IffyOrderOptionId (u "88163569-b0f8-4635-85db-d26545ad052a")]
    let orderPayload = IffyOrderPayload
          { items   = orderItems
          , address = IffyAddress "Paradise City, Green Grass Street"
          }
    let resolvedLocations =
          [(Address "Paradise City, Green Grass Street", Coordinates 10 20)]
    let availableRestaurants =
          [ Restaurant
            { id = RestaurantId (u "20557362-2281-4994-8df9-d7d81395e726")
            , name = "Paradise City"
            , coordinates = Coordinates 11 21
            }
          , Restaurant
            { id = RestaurantId (u "df2cd899-834c-427c-863c-0e9922402243")
            , name = "Ivory Tower"
            , coordinates = Coordinates 12 22
            }
          ]
    let expectedRid = RestaurantId (u "20557362-2281-4994-8df9-d7d81395e726")
    let (result, _) =
          runTest
            $  (Test . setFixtures)
                 [ ResolveAddress resolvedLocations
                 , GetAllRestaurants availableRestaurants
                 ]
            >> placeOrder orderPayload
    restaurantId (right result) `shouldBe` expectedRid

  it "should insert order upon success" $ do
    let orderItems = fromList
          [IffyOrderOptionId (u "88163569-b0f8-4635-85db-d26545ad052a")]
    let orderPayload = IffyOrderPayload
          { items   = orderItems
          , address = IffyAddress "Paradise City, Green Grass Street"
          }
    let (_, testState) = runTest $ placeOrder orderPayload
    testEvents testState `shouldContain` [InsertedOrder]

  it "should reject if address is ambiguous" $ do
    let orderItems = fromList
          [IffyOrderOptionId (u "88163569-b0f8-4635-85db-d26545ad052a")]
    let orderPayload = IffyOrderPayload
          { items   = orderItems
          , address = IffyAddress "Paradise City, Green Grass Street"
          }
    let resolvedLocations =
          [ (Address "Paradise City, Green Grass Street", Coordinates 10 20)
          , (Address "Paradise City, Blue Sky Street"   , Coordinates 10 21)
          ]
    let (result, _) =
          runTest
            $  (Test . setFixtures) [ResolveAddress resolvedLocations]
            >> placeOrder orderPayload
    left result `shouldBe` AmbiguousAddress (fst <$> fromList resolvedLocations)

  it "should reject if address is not found" $ do
    let orderItems = fromList
          [IffyOrderOptionId (u "88163569-b0f8-4635-85db-d26545ad052a")]
    let orderPayload = IffyOrderPayload
          { items   = orderItems
          , address = IffyAddress "Paradise City, Green Grass Street"
          }
    let (result, _) =
          runTest
            $  (Test . setFixtures) [ResolveAddress []]
            >> placeOrder orderPayload
    left result `shouldBe` AddressNotFound

  it "should reject if no restaurants are available" $ do
    let orderItems = fromList
          [IffyOrderOptionId (u "88163569-b0f8-4635-85db-d26545ad052a")]
    let orderPayload = IffyOrderPayload
          { items   = orderItems
          , address = IffyAddress "Paradise City, Green Grass Street"
          }
    let (result, _) =
          runTest
            $  (Test . setFixtures) [GetAllRestaurants []]
            >> placeOrder orderPayload
    left result `shouldBe` NoRestaurantsAvailable

  it "should reject non-existing order options" $ do
    let orderedItems = fromList
          [ IffyOrderOptionId (u "88163569-b0f8-4635-85db-d26545ad052a")
          , IffyOrderOptionId (u "631c225f-0a0d-4b04-a937-d87d50005698")
          ]
    let validatedItems = fromList
          [ Just (OrderOptionId (u "88163569-b0f8-4635-85db-d26545ad052a"))
          , Nothing
          ]
    let orderPayload = IffyOrderPayload
          { items   = orderedItems
          , address = IffyAddress "Paradise City, Green Grass Street"
          }
    let (result, _) =
          runTest
            $  (Test . setFixtures) [CheckOrderOptionsExistence validatedItems]
            >> placeOrder orderPayload
    left result `shouldBe` UnknownOrderOption
      (IffyOrderOptionId (u "631c225f-0a0d-4b04-a937-d87d50005698"))

data Fixtures = GenerateUUID UUID
    | GetAllRestaurants [Restaurant]
    | CheckOrderOptionsExistence (NonEmpty (Maybe OrderOptionId))
    | ResolveAddress [Location]
    deriving (Eq)

data Events = InsertedOrder
    deriving (Eq, Show)

type OrderTest = TestState Events Fixtures
newtype Test m = Test { unTest :: State OrderTest m } deriving (Functor, Applicative, Monad)

instance AddressResolver Test where
  resolveAddress _ = (Test . withFixture defaultLocations) $ \fs -> do
    f@(ResolveAddress v) <- fs
    pure (v, f)
   where
    defaultLocations =
      [(Address "Paradise City, Green Grass Street", Coordinates 10 20)]

instance Order.Persistence.Repo Test where
  insert _ = do
    (Test . putEvent) InsertedOrder
    pure ()

instance Restaurant.Service Test where
  getAll = (Test . withFixture [defaultRestaurant]) $ \fs -> do
    f@(GetAllRestaurants v) <- fs
    pure (v, f)
   where
    defaultRestaurant = Restaurant
      { id          = RestaurantId (u "20557362-2281-4994-8df9-d7d81395e726")
      , name        = "Paradise City"
      , coordinates = Coordinates 11 21
      }

instance OrderOption.Service Test where
  checkExistence ids = (Test . withFixture defaultVal) $ \fs -> do
    f@(CheckOrderOptionsExistence v) <- fs
    pure (v, f)
   where
    defaultVal = fmap markValid ids
    markValid  = Just . OrderOptionId . unIffyOrderOptionId

instance Concurrent Test where
  concurrently3 a b c = do
    a' <- a
    b' <- b
    c' <- c
    pure (a', b', c')

instance UUIDGen Test where
  nextUUID = (Test . withFixture (error "No default UUID")) $ \fs -> do
    f@(GenerateUUID v) <- fs
    pure (v, f)

runTest :: Test a -> (a, OrderTest)
runTest app = runState (unTest app) (TestState [] [])
