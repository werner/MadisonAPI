{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE DeriveGeneric        #-}

module Api.Warehouse where

import           GHC.Generics                     (Generic)
import           Data.Maybe                       (fromMaybe)
import           Data.Aeson                       (ToJSON, FromJSON)
import           Data.Text                        as Text
import           Data.Int                         (Int64)
import           Database.Persist.Postgresql      (insertBy, entityVal, updateWhere, deleteWhere, 
                                                   PersistField(..),
                                                   Entity (..), fromSqlKey, insert, toSqlKey, update,
                                                   delete, selectFirst, selectList, (==.), (=.))
import qualified Database.Esqueleto               as E
import           Network.Wai                      (Application)
import           Servant                          
import           Web.HttpApiData                  (showTextData)

import           Api.Types
import           Config                           (App (..), Config (..))
import           Models
import           Api.User
import qualified Api.Register                     as Register

type RawWarehouseStock = (E.Value Int, E.Value String, 
                          E.Value (Key User), E.Value (Maybe Double))

data WarehouseStock = WarehouseStock { wId     :: Int
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

data SortWarehouse = SWarehouseNameAsc 
                   | SWarehouseNameDesc 
                   | SWarehouseScopedIdAsc 
                   | SWarehouseScopedIdDesc 
                   deriving (Show, Read, Generic)

instance ToJSON   SortWarehouse
instance FromJSON SortWarehouse

instance FromHttpApiData SortWarehouse where
        parseUrlPiece sortWarehouse = Right (read $ Text.unpack sortWarehouse :: SortWarehouse)

instance ToHttpApiData SortWarehouse where
        toUrlPiece = showTextData

data FilterWarehouse = FilterWarehouse { filterName :: Maybe String 
                                       , filterId   :: Maybe Int }
                                       deriving (Show, Read, Generic)

instance ToJSON   FilterWarehouse
instance FromJSON FilterWarehouse

instance FromHttpApiData FilterWarehouse where
        parseUrlPiece filterWarehouse = Right (read $ Text.unpack filterWarehouse :: FilterWarehouse)

instance ToHttpApiData FilterWarehouse where
        toUrlPiece = showTextData

type API = 
             "warehouses" :> MadisonAuthProtect 
                          :> QueryParams "sortField" SortWarehouse
                          :> QueryParam  "limit"     Int64 
                          :> QueryParam  "offset"    Int64          
                          :> ReqBody '[JSON] FilterWarehouse        :> Get    '[JSON] [WarehouseStock]
        :<|> "warehouses" :> MadisonAuthProtect
                          :> ReqBody '[JSON] CrudWarehouse          :> Post   '[JSON] Int
        :<|> "warehouses" :> MadisonAuthProtect
                          :> Capture "id" Int
                          :> ReqBody '[JSON] CrudWarehouse          :> Put    '[JSON] Int
        :<|> "warehouses" :> MadisonAuthProtect
                          :> Capture "id" Int                       :> Delete '[JSON] Int

server :: ServerT Api.Warehouse.API App
server = all' :<|> insert' :<|> update' :<|> delete'

all' :: MadisonAuthData -> [SortWarehouse] -> Maybe Int64 -> Maybe Int64 -> FilterWarehouse -> App [WarehouseStock]
all' session sortWarehouses limit offset filters = do
        warehouses <- findAll' sortWarehouses limit offset filters
        return $ transformAll' warehouses

insert' :: MadisonAuthData -> CrudWarehouse -> App Int
insert' showUser crudWarehouse = do
    user     <- runDb (selectFirst [SessionCookie ==. suId showUser] []) >>= Api.User.getUserBySession
    scopedId <- nextScopedId (fromSqlKey $ entityKey user) WarehouseUserId WarehouseScopedId
    new      <- runDb $ insertBy $ Warehouse (cwName crudWarehouse) (entityKey user) 
                                                scopedId Nothing Nothing
    case new of
        Left  err -> throwError (err409 { errReasonPhrase = "Duplicate warehouse: " 
                                                            Prelude.++ show (warehouseName $ entityVal err) }) 
        Right key -> return scopedId

update' :: MadisonAuthData -> Int -> CrudWarehouse -> App Int
update' showUser id warehouse = do
    user <- runDb (selectFirst [SessionCookie ==. suId showUser] []) >>= Api.User.getUserBySession
    runDb $ updateWhere [WarehouseScopedId ==. id, 
                           WarehouseUserId ==. entityKey user] [WarehouseName =. cwName warehouse] 
    return id

delete' :: MadisonAuthData -> Int -> App Int
delete' showUser id = do
    user <- runDb (selectFirst [SessionCookie ==. suId showUser] []) >>= Api.User.getUserBySession
    runDb $ deleteWhere [WarehouseScopedId ==. id, WarehouseUserId ==. entityKey user]
    return id

getSortField :: E.SqlExpr (Entity Warehouse) -> SortWarehouse -> E.SqlExpr E.OrderBy
getSortField warehouses SWarehouseNameAsc       = E.asc  $ warehouses E.^. WarehouseName
getSortField warehouses SWarehouseNameDesc      = E.desc $ warehouses E.^. WarehouseName
getSortField warehouses SWarehouseScopedIdAsc   = E.asc  $ warehouses E.^. WarehouseScopedId
getSortField warehouses SWarehouseScopedIdDesc  = E.desc $ warehouses E.^. WarehouseScopedId

mapFilterWarehouse
  :: E.Esqueleto query expr backend =>
     expr (Entity Warehouse) -> FilterWarehouse -> query ()
mapFilterWarehouse warehouses (FilterWarehouse (Just name) Nothing)   = E.where_ $ warehouses E.^. WarehouseName `E.ilike`
                                                                          (E.%) E.++. E.val name E.++. (E.%)
mapFilterWarehouse warehouses (FilterWarehouse Nothing (Just id))     = E.where_ $ warehouses E.^. WarehouseScopedId E.==. 
                                                                          E.val id
mapFilterWarehouse warehouses (FilterWarehouse (Just name) (Just id)) = E.where_ $ 
                                                                            (warehouses E.^. WarehouseName `E.ilike`
                                                                              (E.%) E.++. E.val name E.++. (E.%))
                                                                          E.&&.
                                                                            warehouses E.^. WarehouseScopedId E.==. E.val id
mapFilterWarehouse warehouses (FilterWarehouse Nothing Nothing)       = E.where_ $ warehouses E.^. WarehouseName `E.ilike`
                                                                          (E.%) E.++. E.val "%" E.++. (E.%) 

findAll' :: [SortWarehouse] -> Maybe Int64 -> Maybe Int64 -> FilterWarehouse -> App [RawWarehouseStock]
findAll' sortWarehouses limit offset filters = runDb 
                        $ E.select 
                        $ E.from $ \(warehouses `E.LeftOuterJoin` stocks) -> do
                            E.on $ E.just (warehouses E.^. WarehouseId) E.==. stocks E.?. StockWarehouseId
                            mapFilterWarehouse warehouses filters
                            E.orderBy $ Prelude.map  (getSortField warehouses) sortWarehouses
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
transform' warehouse = WarehouseStock (E.unValue id) 
                                      (E.unValue name)
                                      (fromSqlKey $ E.unValue userId) 
                                      (fromMaybe 0 $ E.unValue stock)
                          where (id, name, userId, stock) = warehouse

transformAll' :: [RawWarehouseStock] -> [WarehouseStock]
transformAll' = Prelude.map transform'
