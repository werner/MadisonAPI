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

import           Database.Persist.Audit.Queries

import           Api.Types
import           Config                           (App (..), Config (..))
import           Models.Base
import           Models.Warehouse
import           Models.User
import qualified Api.Register                     as Register

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
    user     <- runDb (selectFirst [SessionCookie ==. suId showUser] []) >>= getUserBySession
    scopedId <- nextScopedId (fromSqlKey $ entityKey user) WarehouseUserId WarehouseScopedId
    new      <- runDb $ insertAndAuditBy ( Warehouse (cwName crudWarehouse) (entityKey user) scopedId )
                                         ( Text.pack $ userEmail $ entityVal user )
    case new of
        Left  err -> throwError (err409 { errReasonPhrase = "Duplicate warehouse: " 
                                                            Prelude.++ show (warehouseName $ entityVal err) }) 
        Right key -> return scopedId

update' :: MadisonAuthData -> Int -> CrudWarehouse -> App Int
update' showUser id warehouse = do
    user <- runDb (selectFirst [SessionCookie ==. suId showUser] []) >>= getUserBySession
    runDb $ updateWhereAndAudit [WarehouseScopedId ==. id, 
                                 WarehouseUserId   ==. entityKey user] 
                                [WarehouseName =. cwName warehouse] 
                                ( Text.pack $ userEmail $ entityVal user )
    return id

delete' :: MadisonAuthData -> Int -> App Int
delete' showUser id = do
    user <- runDb (selectFirst [SessionCookie ==. suId showUser] []) >>= getUserBySession
    runDb $ deleteWhereAndAudit [WarehouseScopedId ==. id, WarehouseUserId ==. entityKey user]
                                ( Text.pack $ userEmail $ entityVal user )
    return id
