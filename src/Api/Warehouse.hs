{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE DeriveGeneric #-}
module Api.Warehouse where

import           GHC.Generics
import           Data.Maybe
import           Data.Aeson
import           Data.Vector                 as V
import           Data.Text                   as Text
import           Data.Int                    (Int64)
import           Database.Persist.Postgresql as P
import           Database.Persist.Postgresql (Entity (..), fromSqlKey, insert, toSqlKey, update,
                                              delete, selectFirst, selectList, (==.), (=.))
import qualified Database.Esqueleto          as E
import           Database.Esqueleto          ((^.), (?.))
import           Network.Wai                 (Application)
import           Servant

import           Config                      (App (..), Config (..))
import           Models

type RawWarehouseStock = (E.Value (Key Warehouse), E.Value String, 
                          E.Value (Key User), E.Value (Maybe Double))

data WarehouseStock = WarehouseStock { wId     :: Int64
                                     , wName   :: String
                                     , wUserId :: Int64
                                     , wStock  :: Double
                                     }
                                     deriving (Show, Generic)

instance ToJSON WarehouseStock

data SortOrder = SAsc | SDesc deriving (Read, Show, Generic)

instance FromHttpApiData SortOrder where
        parseUrlPiece sortOrder = Right (read $ Text.unpack sortOrder :: SortOrder)

type API = 
             "warehouses" :> QueryParam "name"   String 
                          :> QueryParam "order"  SortOrder                   
                          :> QueryParam "limit"  Int64 
                          :> QueryParam "offset" Int64                       :> Get    '[JSON] [WarehouseStock]
        :<|> "warehouses" :> Capture "id" Int64                              :> Get    '[JSON] (Entity Warehouse)
        :<|> "warehouses" :> ReqBody '[JSON] Warehouse                       :> Post   '[JSON] Int64
        :<|> "warehouses" :> Capture "id" Int64 :> ReqBody '[JSON] Warehouse :> Put    '[JSON] Int64
        :<|> "warehouses" :> Capture "id" Int64                              :> Delete '[JSON] Int64

server :: ServerT API App
server = all' :<|> show' :<|> insert' :<|> update' :<|> delete'

all' :: Maybe String -> Maybe SortOrder -> Maybe Int64 -> Maybe Int64 -> App [WarehouseStock]
all' name sortMethod limit offset = do
        warehouses <- findAll' name sortMethod limit offset
        return $ transformAll' warehouses

show' :: Int64 -> App (Entity Warehouse)
show' id = do
    maybeWarehouse <- runDb (selectFirst [ WarehouseId P.==. toSqlKey id] [])
    getWarehouse maybeWarehouse

insert' :: Warehouse -> App Int64
insert' warehouse = do
    new <- runDb $ P.insert $ Warehouse (warehouseName warehouse) (warehouseUserId warehouse) Nothing Nothing
    return $ fromSqlKey new

update' :: Int64 -> Warehouse -> App Int64
update' id warehouse = do
    warehouseKey <- getKeyFromId id
    runDb $ P.update warehouseKey [WarehouseName =. warehouseName warehouse]
    return $ fromSqlKey warehouseKey

delete' :: Int64 -> App Int64
delete' id = do
    warehouseKey <- getKeyFromId id
    runDb $ P.delete warehouseKey
    return $ fromSqlKey warehouseKey

getKeyFromId :: Int64 -> App (Key Warehouse)
getKeyFromId id = do
    maybeWarehouse   <- runDb (selectFirst [ WarehouseId P.==. toSqlKey id] [])
    warehouse'       <- getWarehouse maybeWarehouse
    return $ entityKey warehouse'

getWarehouse :: Maybe (Entity Warehouse) -> App (Entity Warehouse)
getWarehouse Nothing           = throwError err404
getWarehouse (Just warehouse') = return warehouse'

getSortMethod :: (PersistField t) => Maybe SortOrder -> (E.SqlExpr (E.Value t) -> E.SqlExpr E.OrderBy)
getSortMethod (Just SAsc)  = E.asc
getSortMethod (Just SDesc) = E.desc
getSortMethod Nothing      = E.asc

findAll' :: Maybe String -> Maybe SortOrder -> Maybe Int64 -> Maybe Int64 -> App [RawWarehouseStock]
findAll' name sortMethod limit offset = runDb 
                        $ E.select 
                        $ E.from $ \(warehouses `E.LeftOuterJoin` stocks) -> do
                            E.on $ E.just (warehouses ^. WarehouseId) E.==. stocks ?. StockWarehouseId
                            E.where_  $ warehouses ^. WarehouseName `E.ilike`
                                        (E.%) E.++. E.val (fromMaybe "%" name) E.++. (E.%)
                            E.orderBy [getSortMethod sortMethod (warehouses ^. WarehouseName)]
                            E.groupBy (warehouses ^. WarehouseId,
                                       warehouses ^. WarehouseName,
                                       warehouses ^. WarehouseUserId)
                            E.limit  $ fromMaybe 10 limit
                            E.offset $ fromMaybe 0  offset
                            return
                                ( warehouses ^. WarehouseId
                                , warehouses ^. WarehouseName 
                                , warehouses ^. WarehouseUserId
                                , E.sum_ (stocks ?. StockAmount)
                                )

transform' :: RawWarehouseStock -> WarehouseStock
transform' warehouse = WarehouseStock (fromSqlKey $ E.unValue id) 
                                      (E.unValue name)
                                      (fromSqlKey $ E.unValue userId) 
                                      (fromMaybe 0 $ E.unValue stock)
                          where (id, name, userId, stock) = warehouse

transformAll' :: [RawWarehouseStock] -> [WarehouseStock]
transformAll' = Prelude.map transform'
