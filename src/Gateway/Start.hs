{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}

module Gateway.Start where

import Base.Coordinates
import Base.ResolveAddress
import Data.Aeson
import Data.List.NonEmpty
import qualified Data.UUID.V4 as UUID
import Database.PostgreSQL.Simple
import Order
import Order.PlaceOrder
import OrderOption
import OrderOption.Register
import Restaurant
import Yesod

data App = App

mkYesod "App" [parseRoutes|
/orders OrdersR POST
/orderOptions OrderOptionsR POST
|]

instance Yesod App

postOrdersR :: HandlerFor App Value
postOrdersR = do
    order <- requireCheckJsonBody
    request <- liftIO $ placeOrder' order
    pure $ toJSON request

postOrderOptionsR :: HandlerFor App ()
postOrderOptionsR = do
    orderOption <- requireCheckJsonBody
    liftIO $ registerOrderOption' orderOption
    pure ()


startGateway = warp 3000 App

registerOrderOption' :: RegisterOrderOption IO
registerOrderOption' optionPayload = do
  conn <- connect defaultConnectInfo
      { connectHost = "localhost"
      , connectDatabase = "PizzaDelivery"
      , connectUser = "postgres"
      , connectPassword = "admin"
      }
  registerOrderOption conn optionPayload

-- MOCK IMPLEMENTATIONS

placeOrder' :: PlaceOrder IO
placeOrder' = placeOrder resolveAddress' getAllRestaurants' (RequestId <$> UUID.nextRandom)

resolveAddress' :: ResolveAddress IO
resolveAddress' _ = pure (Coordinates 28 53)

getAllRestaurants' :: GetAllRestaurants IO
getAllRestaurants' = pure mockRestaurants
  where
    mockRestaurants = mockRestaurant :| []
    mockRestaurant = Restaurant (RestaurantId "000-000-001") (Coordinates 27 53)

