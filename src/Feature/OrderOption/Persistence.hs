{-# LANGUAGE OverloadedStrings #-}
module Feature.OrderOption.Persistence
    ( insertOrderOption
    , queryAllOrderOptions
    ) where

import Base.PG
import Control.Exception
import Control.Monad.IO.Class
import Data.Aeson
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToRow
import Feature.OrderOption.Types

queryAllOrderOptions :: MonadIO m => m [OrderOption]
queryAllOrderOptions = do
    results <- withConn $ \conn -> query_ conn "SELECT id, name, sizes from OrderOptions"
    pure $ fmap unOrderOptionEntity results

insertOrderOption :: MonadIO m => OrderOption -> m (Either RegisterOptionError ())
insertOrderOption option = do
    let result = withConn $ \conn -> execute conn insertQuery (OrderOptionEntity option)
    liftIO $ catch (Right () <$ result) catchSqlException
      where
    insertQuery = "INSERT INTO OrderOptions (id, name, sizes) VALUES (?, ?, ?)"
    catchSqlException sqlError
        | sqlState sqlError == "23505" = pure $ Left NameAlreadyInUse
        | otherwise = throw sqlError


newtype OrderOptionEntity = OrderOptionEntity { unOrderOptionEntity :: OrderOption }

instance ToRow OrderOptionEntity where
    toRow (OrderOptionEntity orderOption) =
        let (OrderOption (OrderOptionId oid) (Pizza name sizes)) = orderOption in
        toRow (oid, name, toJSON $ map PizzaSizeEntity sizes)

instance FromRow OrderOptionEntity where
    fromRow = do
        oid <- field; name <- field; sizes <- field

        let parsedSizes = case fromJSON sizes of
                Success sizesEntities -> fmap unPizzaSizeEntity sizesEntities
                Error message -> error $ mconcat ["Invalid OrderOptionEntity: ", message]

        pure $ OrderOptionEntity $ OrderOption
            { orderOptionId = OrderOptionId oid
            , orderOptionPayload = Pizza name parsedSizes
            }

newtype PizzaSizeEntity = PizzaSizeEntity { unPizzaSizeEntity :: PizzaSize }

instance FromJSON PizzaSizeEntity where
    parseJSON = withObject "PizzaSizeEntity" $ \v -> do
        size <- PizzaSize
            <$> v .: "diameter"
            <*> v .: "cost"
        pure $ PizzaSizeEntity size

instance ToJSON PizzaSizeEntity where
    toJSON (PizzaSizeEntity (PizzaSize diameter cost)) = object
        [ "diameter" .= diameter
        , "cost" .= cost
        ]
