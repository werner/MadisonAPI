{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE DeriveGeneric        #-}

module Api.Warehouse where

import           Control.Monad.Reader.Class
import           Control.Monad.IO.Class
import           GHC.Generics
import           Data.Maybe
import           Data.Aeson
import           Data.Vector                      as V
import           Data.Text                        as Text
import           Data.Int                         (Int64)
import           Database.Persist.Postgresql      (insertBy, entityVal, updateWhere, deleteWhere, 
                                                   PersistField(..),
                                                   Entity (..), fromSqlKey, insert, toSqlKey, update,
                                                   delete, selectFirst, selectList, (==.), (=.))
import qualified Database.Esqueleto               as E
import           Network.Wai                      (Application)
import           Servant
import           Web.HttpApiData

import           Api.Types
import           Config                           (App (..), Config (..))
import           Models
import           Api.User
import qualified Api.Register                     as Register

type RawWarehouseStock = (E.Value (Key Warehouse), E.Value String, 
                          E.Value (Key User), E.Value (Maybe Double))

data WarehouseStock = WarehouseStock { wId     :: Int64
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

data SortField = SWarehouseName | SWarehouseScopedId deriving (Read, Show, Generic)

instance FromHttpApiData SortField where
        parseUrlPiece sortField = Right (read $ Text.unpack sortField :: SortField)

instance ToHttpApiData SortField where
        toUrlPiece = showTextData

data SortWarehouse = SortWarehouse { sSortOrder :: SortOrder
                                   , sSortField :: SortField }
                                   deriving (Show, Read, Generic)

instance FromHttpApiData [SortWarehouse] where
        parseUrlPiece sortWarehouse = Right (read $ Text.unpack sortWarehouse :: [SortWarehouse])

instance ToHttpApiData [SortWarehouse] where
        toUrlPiece = showTextData

type API = 
             "warehouses" :> MadisonAuthProtect 
                          :> QueryParam "name"      String 
                          :> QueryParam "sortField" [SortWarehouse]
                          :> QueryParam "limit"     Int64 
                          :> QueryParam "offset"    Int64  :> Get    '[JSON] [WarehouseStock]
        :<|> "warehouses" :> MadisonAuthProtect 
                          :> Capture "id" Int              :> Get    '[JSON] (Entity Warehouse)
        :<|> "warehouses" :> MadisonAuthProtect
                          :> ReqBody '[JSON] CrudWarehouse :> Post   '[JSON] Int
        :<|> "warehouses" :> MadisonAuthProtect
                          :> Capture "id" Int
                          :> ReqBody '[JSON] CrudWarehouse :> Put    '[JSON] Int
        :<|> "warehouses" :> MadisonAuthProtect
                          :> Capture "id" Int              :> Delete '[JSON] Int

server :: ServerT Api.Warehouse.API App
server = all' :<|> show' :<|> insert' :<|> update' :<|> delete'

all' :: MadisonAuthData -> Maybe String -> Maybe [SortWarehouse] 
                        -> Maybe Int64 -> Maybe Int64 -> App [WarehouseStock]
all' session name sortWarehouses limit offset = do
        warehouses <- findAll' name sortWarehouses limit offset
        return $ transformAll' warehouses

show' :: MadisonAuthData -> Int -> App (Entity Warehouse)
show' showUser id = do
    user           <- runDb (selectFirst [SessionCookie ==. suId showUser] []) >>= Api.User.getUserBySession
    maybeWarehouse <- runDb (selectFirst [WarehouseScopedId ==. id, WarehouseUserId ==. entityKey user] [])
    getWarehouse maybeWarehouse

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

getWarehouse :: Maybe (Entity Warehouse) -> App (Entity Warehouse)
getWarehouse Nothing           = throwError err404
getWarehouse (Just warehouse') = return warehouse'

getSortField :: E.SqlExpr (Entity Warehouse) -> SortOrder -> SortField -> E.SqlExpr E.OrderBy
getSortField warehouses sortOrder SWarehouseName     = getSortMethod sortOrder $ warehouses E.^. WarehouseName
getSortField warehouses sortOrder SWarehouseScopedId = getSortMethod sortOrder $ warehouses E.^. WarehouseScopedId

findAll' :: Maybe String -> Maybe [SortWarehouse]
                         -> Maybe Int64 -> Maybe Int64 -> App [RawWarehouseStock]
findAll' name sortWarehouses limit offset = runDb 
                        $ E.select 
                        $ E.from $ \(warehouses `E.LeftOuterJoin` stocks) -> do
                            E.on $ E.just (warehouses E.^. WarehouseId) E.==. stocks E.?. StockWarehouseId
                            E.where_  $ warehouses E.^. WarehouseName `E.ilike`
                                        (E.%) E.++. E.val (fromMaybe "%" name) E.++. (E.%)
                            E.orderBy $ Prelude.map (\x -> getSortField warehouses 
                                                             (sSortOrder x) (sSortField x)) $
                                       fromMaybe [SortWarehouse SAsc SWarehouseScopedId] sortWarehouses
                            E.groupBy (warehouses E.^. WarehouseId,
                                       warehouses E.^. WarehouseName,
                                       warehouses E.^. WarehouseUserId)
                            E.limit  $ fromMaybe 10 limit
                            E.offset $ fromMaybe 0  offset
                            return
                                ( warehouses E.^. WarehouseId
                                , warehouses E.^. WarehouseName 
                                , warehouses E.^. WarehouseUserId
                                , E.sum_ (stocks E.?. StockAmount)
                                )

transform' :: RawWarehouseStock -> WarehouseStock
transform' warehouse = WarehouseStock (fromSqlKey $ E.unValue id) 
                                      (E.unValue name)
                                      (fromSqlKey $ E.unValue userId) 
                                      (fromMaybe 0 $ E.unValue stock)
                          where (id, name, userId, stock) = warehouse

transformAll' :: [RawWarehouseStock] -> [WarehouseStock]
transformAll' = Prelude.map transform'
