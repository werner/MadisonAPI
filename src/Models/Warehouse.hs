{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE DeriveGeneric        #-}
module Models.Warehouse where

import           GHC.Generics                     (Generic)
import           Data.Maybe                       (fromMaybe)
import           Data.Text
import qualified Database.Esqueleto               as E
import           Data.Int                         (Int64)
import           Data.Aeson                       (ToJSON, FromJSON)
import           Web.HttpApiData                  (FromHttpApiData, ToHttpApiData, parseUrlPiece
                                                  , toUrlPiece, showTextData)
import           Database.Persist.Postgresql      (Entity (..), fromSqlKey)

import           Database.Persist.Audit.Types
import           Database.Persist.Audit.Class

import           Models.Base
import           Config

type RawWarehouseStock = (E.Value Int, E.Value String, 
                          E.Value (Key User), E.Value (Maybe Double))

data WarehouseStock 
        = WarehouseStock 
        { wId     :: Int
        , wName   :: String
        , wUserId :: Int64
        , wStock  :: Double
        }
        deriving (Eq, Show, Read, Generic)

data CrudWarehouse = CrudWarehouse { cwName :: String } deriving (Eq, Show, Read, Generic)

instance ToJSON WarehouseStock
instance FromJSON WarehouseStock

instance ToJSON CrudWarehouse
instance FromJSON CrudWarehouse

data SortWarehouse  = SWarehouseName SortOrder
                    | SWarehouseScopedId SortOrder
                    deriving (Generic, Read, Show)

instance ToJSON   SortWarehouse
instance FromJSON SortWarehouse

instance FromHttpApiData SortWarehouse where
        parseUrlPiece sortWarehouse = 
            case (unpack sortWarehouse) of
                "name-asc"  -> Right (SWarehouseName SAsc)
                "name-desc" -> Right (SWarehouseName SDesc)
                "id-asc"    -> Right (SWarehouseScopedId SAsc)
                "id-desc"   -> Right (SWarehouseScopedId SDesc)

instance ToHttpApiData SortWarehouse where
        toUrlPiece = showTextData

instance ToAudit Warehouse where
        type AuditResult Warehouse = WarehouseAudit
        toAudit v k auditAction editedBy editedOn = WarehouseAudit (warehouseName v) (warehouseUserId v)
                                                                   (warehouseScopedId v)
                                                                   k auditAction editedBy editedOn

getSortField :: E.SqlExpr (Entity Warehouse) -> SortWarehouse -> E.SqlExpr E.OrderBy
getSortField warehouses (SWarehouseScopedId SAsc)  = E.asc  $ warehouses E.^. WarehouseScopedId
getSortField warehouses (SWarehouseScopedId SDesc) = E.desc $ warehouses E.^. WarehouseScopedId
getSortField warehouses (SWarehouseName SAsc)      = E.asc  $ warehouses E.^. WarehouseName
getSortField warehouses (SWarehouseName SDesc)     = E.desc $ warehouses E.^. WarehouseName

nameCondition
  :: E.Esqueleto query expr backend 
  => expr (Entity Warehouse) -> String -> expr (E.Value Bool)
nameCondition warehouses name = warehouses E.^. WarehouseName `E.ilike` (E.%) E.++. E.val name E.++. (E.%)

idCondition
  :: E.Esqueleto query expr backend 
  => expr (Entity Warehouse) -> Int -> expr (E.Value Bool)
idCondition   warehouses id   = warehouses E.^. WarehouseScopedId E.==. E.val id

allCondition
  :: E.Esqueleto query expr backend 
  => expr (Entity Warehouse) -> expr (E.Value Bool)
allCondition  warehouses      = warehouses E.^. WarehouseName `E.ilike` (E.%) E.++. E.val "%" E.++. (E.%)

mapFilterWarehouse
  :: E.Esqueleto query expr backend 
  => expr (Entity Warehouse) 
  -> Maybe String
  -> Maybe Int
  -> query ()
mapFilterWarehouse warehouses (Just name) (Just id) = E.where_ $ nameCondition warehouses name
                                                        E.&&. idCondition warehouses id
mapFilterWarehouse warehouses Nothing (Just id)     = E.where_ $ idCondition warehouses id
mapFilterWarehouse warehouses (Just name) Nothing   = E.where_ $ nameCondition warehouses name
mapFilterWarehouse warehouses Nothing Nothing       = E.where_ $ allCondition warehouses

findAll' :: [SortWarehouse] -> Maybe Int64 -> Maybe Int64 -> Maybe String -> Maybe Int -> App [RawWarehouseStock]
findAll' sortWarehouses limit offset filterName filterId = runDb 
                        $ E.select 
                        $ E.from $ \(warehouses `E.LeftOuterJoin` stocks) -> do
                            E.on $ E.just (warehouses E.^. WarehouseId) E.==. stocks E.?. StockWarehouseId
                            mapFilterWarehouse warehouses filterName filterId
                            E.orderBy $ Prelude.map (getSortField warehouses) sortWarehouses
                            E.groupBy (warehouses E.^. WarehouseId,
                                       warehouses E.^. WarehouseName,
                                       warehouses E.^. WarehouseUserId)
                            E.limit  $ fromMaybe 10 limit
                            E.offset $ fromMaybe 0  offset
                            return
                                ( warehouses E.^. WarehouseScopedId
                                , warehouses E.^. WarehouseName 
                                , warehouses E.^. WarehouseUserId
                                , E.sum_ (stocks E.?. StockAmount)
                                )

transform' :: RawWarehouseStock -> WarehouseStock
transform' (id, name, userId, stock) = 
        WarehouseStock (E.unValue id) 
                       (E.unValue name)
                       (fromSqlKey $ E.unValue userId) 
                       (fromMaybe 0 $ E.unValue stock)

transformAll' :: [RawWarehouseStock] -> [WarehouseStock]
transformAll' = Prelude.map transform'
