{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}

module Gateway.Start where

import Base.Coordinates (Coordinates (Coordinates))
import Base.ResolveAddress (ResolveAddress)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value, toJSON)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text (Text)
import qualified Data.UUID.V4 as UUID
import Database.PostgreSQL.Simple
    ( Connection
    , connect
    , connectDatabase
    , connectHost
    , connectPassword
    , connectUser
    , defaultConnectInfo
    )
import Gateway.Dto.OrderOption (orderOptionToDto)
import Network.HTTP.Types (conflict409, created201)
import Order (PlaceOrder, ProcessOrderRequestId (RequestId))
import Order.Service (placeOrder)
import OrderOption
    ( GetAllOrderOptions
    , RegisterOptionError (NameAlreadyInUse)
    , RegisterOrderOption
    )
import OrderOption.Persistence (insertOrderOption, queryAllOrderOptions)
import OrderOption.Service (registerOrderOption)
import Restaurant
    (GetAllRestaurants, Restaurant (Restaurant), RestaurantId (RestaurantId))
import Yesod
    ( HandlerFor
    , Yesod
    , mkYesod
    , parseRoutes
    , renderRoute
    , requireCheckJsonBody
    , sendResponseStatus
    , warp
    )

data App = App

mkYesod "App" [parseRoutes|
/orders OrdersR POST
/orderOptions OrderOptionsR GET POST
|]

instance Yesod App

postOrdersR :: HandlerFor App Value
postOrdersR = do
    order <- requireCheckJsonBody
    request <- liftIO $ placeOrder' order
    pure $ toJSON request

getOrderOptionsR :: HandlerFor App Value
getOrderOptionsR = do
    result <- liftIO getAllOrderOptions'
    pure $ toJSON $ map orderOptionToDto result

postOrderOptionsR :: HandlerFor App Value
postOrderOptionsR = do
    payload <- requireCheckJsonBody
    result <- liftIO $ registerOrderOption' payload
    case result of
      (Left NameAlreadyInUse) -> sendResponseStatus conflict409 $ toJSON ("Order option name already in use" :: Text)
      (Right orderOption)     -> sendResponseStatus created201 $ toJSON orderOption

startGateway :: IO ()
startGateway = warp 3000 App

getAllOrderOptions' :: GetAllOrderOptions IO
getAllOrderOptions' =  mkPostgreSQLConnection >>= queryAllOrderOptions

registerOrderOption' :: RegisterOrderOption IO
registerOrderOption' optionPayload = do
  conn <- mkPostgreSQLConnection
  registerOrderOption (insertOrderOption conn) optionPayload

mkPostgreSQLConnection :: IO Connection
mkPostgreSQLConnection = connect defaultConnectInfo
    { connectHost = "localhost"
    , connectDatabase = "PizzaDelivery"
    , connectUser = "postgres"
    , connectPassword = "admin"
    }

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

