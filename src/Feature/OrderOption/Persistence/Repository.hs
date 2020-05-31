{-# LANGUAGE OverloadedStrings #-}
module Feature.OrderOption.Persistence.Repository
    ( insertOrderOption
    , queryAllOrderOptions
    , queryOrderOptionById
    , filterExistingOrderOptionIds
    , deleteOrderOption
    ) where

import Control.Exception
import Control.Monad.IO.Class
import Data.Aeson
import Data.List.NonEmpty
import Data.Maybe
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToRow
import Feature.OrderOption.Types
import Persistence.PG

queryAllOrderOptions :: MonadIO m => m [OrderOption]
queryAllOrderOptions = do
    results <- withConn $ \conn -> query_ conn "SELECT id, name, sizes FROM OrderOptions"
    pure $ fmap unOrderOptionEntity results

queryOrderOptionById :: MonadIO m => OrderOptionId -> m (Maybe OrderOption)
queryOrderOptionById (OrderOptionId ooid) = do
    results <- withConn $ \conn ->
        query conn "SELECT id, name, sizes FROM OrderOptions WHERE id=? LIMIT 1" (Only ooid)
    pure $ unOrderOptionEntity <$> listToMaybe results

filterExistingOrderOptionIds :: MonadIO m => NonEmpty IffyOrderOptionId -> m [OrderOptionId]
filterExistingOrderOptionIds ids = do
    results <- withConn $ \conn -> query conn "SELECT id FROM OrderOptions WHERE id in ?" ids'
    pure $ fmap (OrderOptionId . fromOnly) results
        where
    ids' = Only $ In $ toList (unIffyOrderOptionId <$> ids)

insertOrderOption :: MonadIO m => OrderOption -> m (Either RegisterOptionError ())
insertOrderOption option = do
    let result = withConn $ \conn -> execute conn insertQuery (OrderOptionEntity option)
    liftIO $ catch (Right () <$ result) catchSqlException
      where
    insertQuery = "INSERT INTO OrderOptions (id, name, sizes) VALUES (?, ?, ?)"
    catchSqlException sqlError
        | sqlState sqlError == "23505" = pure $ Left $ NameAlreadyInUse orderOptionName
        | otherwise = throw sqlError
    orderOptionName = pizzaName $ orderOptionPayload option

deleteOrderOption :: MonadIO m => OrderOptionId -> m (Either DeleteOrderOptionError ())
deleteOrderOption (OrderOptionId ooid) = do
    updateCount <- withConn $ \conn -> execute conn "DELETE FROM OrderOptions WHERE id=?" (Only ooid)
    let result = if updateCount == 0
        then Left $ OrderOptionDidNotExist (OrderOptionId ooid)
        else Right ()
    pure result


newtype OrderOptionEntity = OrderOptionEntity { unOrderOptionEntity :: OrderOption }

instance ToRow OrderOptionEntity where
    toRow (OrderOptionEntity orderOption) =
        let (OrderOption (OrderOptionId oid) (Pizza name sizes)) = orderOption in
        toRow (oid, name, toJSON $ fmap PizzaSizeEntity sizes)

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
