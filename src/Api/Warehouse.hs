{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}

module Api.Warehouse where

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


type WarehouseStock = ((E.Value (Key Warehouse)), (E.Value String), 
                       (E.Value (Key User)), (E.Value (Maybe Double)))

instance ToJSON (E.Value (Key Warehouse)) where
        toJSON (E.Value key) = Number $ fromIntegral $ fromSqlKey key

instance ToJSON (E.Value String) where
        toJSON (E.Value string) = String $ Text.pack string

instance ToJSON (E.Value (Maybe Double)) where
        toJSON (E.Value (Just double)) = Number $ fromRational $ toRational double
        toJSON (E.Value Nothing)       = Number 0

instance ToJSON (E.Value (Key User)) where
        toJSON (E.Value key) = Number $ fromIntegral $ fromSqlKey key

type API = 
             "warehouses" :> Get '[JSON] [WarehouseStock]
        :<|> "warehouses" :> Capture "id" Int64                              :> Get    '[JSON] (Entity Warehouse)
        :<|> "warehouses" :> ReqBody '[JSON] Warehouse                       :> Post   '[JSON] Int64
        :<|> "warehouses" :> Capture "id" Int64 :> ReqBody '[JSON] Warehouse :> Put    '[JSON] Int64
        :<|> "warehouses" :> Capture "id" Int64                              :> Delete '[JSON] Int64

server :: ServerT API App
server = all' :<|> show' :<|> insert' :<|> update' :<|> delete'

getWarehouse :: Maybe (Entity Warehouse) -> App (Entity Warehouse)
getWarehouse Nothing           = throwError err404
getWarehouse (Just warehouse') = return warehouse'

all' :: App [WarehouseStock]
all' = runDb 
      $ E.select 
      $ E.from $ \(warehouses `E.LeftOuterJoin` stocks) -> do
          E.on $ E.just (warehouses ^. WarehouseId) E.==. stocks ?. StockWarehouseId
          E.groupBy $ (warehouses ^. WarehouseId, warehouses ^. WarehouseName, warehouses ^. WarehouseUserId)
          return
            ( warehouses ^. WarehouseId
            , warehouses ^. WarehouseName 
            , warehouses ^. WarehouseUserId
            , E.sum_ (stocks ?. StockAmount)
            )

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
    maybeWarehouse   <- runDb (selectFirst [ WarehouseId P.==. toSqlKey id] [])
    warehouse'       <- getWarehouse maybeWarehouse
    let warehouseKey = entityKey warehouse'
    runDb $ P.update warehouseKey [WarehouseName =. warehouseName warehouse]
    return $ fromSqlKey warehouseKey

delete' :: Int64 -> App Int64
delete' id = do
    maybeWarehouse   <- runDb (selectFirst [ WarehouseId P.==. toSqlKey id] [])
    warehouse'       <- getWarehouse maybeWarehouse
    let warehouseKey = entityKey warehouse'
    runDb $ P.delete warehouseKey
    return $ fromSqlKey warehouseKey
