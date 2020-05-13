{-# LANGUAGE OverloadedStrings #-}
module Feature.OrderOption.Persistence
    ( insertOrderOption
    , queryAllOrderOptions
    ) where

import Base.PG
import Control.Exception
import Control.Monad.IO.Class
import Data.Aeson
import qualified Data.Text as T
import Data.UUID
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import Feature.OrderOption.Types

type Id = UUID
type Name = T.Text
type Sizes = Value
type OrderOptionEntity = (Id, Name, Sizes)

queryAllOrderOptions :: MonadIO m => m [OrderOption]
queryAllOrderOptions = do
    results <- withConn $ \conn -> query_ conn "SELECT id, name, sizes from OrderOptions"
    pure $ fmap optionFromEntity results

insertOrderOption :: MonadIO m => OrderOption -> m (Either RegisterOptionError ())
insertOrderOption option = do
    let result = withConn $ \conn -> execute conn insertQuery (optionToEntity option)
    liftIO $ catch (Right () <$ result) catchSqlException
      where
    insertQuery = "INSERT INTO OrderOptions (id, name, sizes) VALUES (?, ?, ?)"
    catchSqlException sqlError
        | sqlState sqlError == "23505" = pure $ Left NameAlreadyInUse
        | otherwise = throw sqlError

optionToEntity :: OrderOption -> [Action]
optionToEntity (OrderOption (OrderOptionId oid) (Pizza name sizes)) =
    toRow (oid, name, sizesToEntity sizes)
  where
    sizesToEntity = encode . map sizeToJSON
    sizeToJSON (PizzaSize diameter cost) = object
        [ "diameter" .= diameter
        , "cost" .= cost
        ]

optionFromEntity :: OrderOptionEntity -> OrderOption
optionFromEntity (oid, name, sizes) = OrderOption
    { orderOptionId = OrderOptionId oid
    , orderOptionPayload = payload
    }
  where
    payload = Pizza
        { pizzaName = name
        , pizzaSizes = case fromJSON sizes of
            Success x -> x
            Error message   -> error $ mconcat ["Invalid OrderOptionEntity: ", message]
        }
