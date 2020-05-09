{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}

module Gateway.Start where

import           Base.Domain
import           Base.ResolveAddress
import           Data.Aeson
import           Data.List.NonEmpty
import qualified Data.UUID.V4        as UUID
import           Order.Domain
import           Order.PlaceOrder
import           Restaurant.Domain
import           Yesod

data App = App

mkYesod "App" [parseRoutes|
/orders OrdersR POST
|]

instance Yesod App

postOrdersR :: HandlerFor App Value
postOrdersR = do
    order <- requireCheckJsonBody  :: Handler Order
    request <- liftIO $ placeOrder' order
    pure $ toJSON request

placeOrder' :: PlaceOrder IO
placeOrder' = placeOrder resolveAddress' getAllRestaurants' (RequestId <$> UUID.nextRandom)

resolveAddress' :: ResolveAddress IO
resolveAddress' _ = pure (Coordinates 28 53)

getAllRestaurants' :: GetAllRestaurants IO
getAllRestaurants' = pure mockRestaurants
  where
    mockRestaurants = mockRestaurant :| []
    mockRestaurant = Restaurant (RestaurantId "000-000-001") (Coordinates 27 53)

startGateway = warp 3000 App
