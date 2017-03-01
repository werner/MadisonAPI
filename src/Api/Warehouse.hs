{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE DeriveGeneric        #-}

module Api.Warehouse where

import           Data.Monoid                      ((<>))
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
                          :> QueryParams "sortField"  SortWarehouse
                          :> QueryParam  "limit"      Int64 
                          :> QueryParam  "offset"     Int64          
                          :> QueryParam  "filterName" String
                          :> QueryParam  "filterId"   Int            :> Get    '[JSON] [WarehouseStock]
        :<|> "warehouses" :> MadisonAuthProtect
                          :> ReqBody '[JSON] CrudWarehouse           :> Post   '[JSON] Int
        :<|> "warehouses" :> MadisonAuthProtect
                          :> Capture "id" Int
                          :> ReqBody '[JSON] CrudWarehouse           :> Put    '[JSON] ()
        :<|> "warehouses" :> MadisonAuthProtect
                          :> Capture "id" Int                        :> Delete '[JSON] ()

server :: ServerT Api.Warehouse.API App
server = all' :<|> insert' :<|> update' :<|> delete'

all' :: MadisonAuthData 
     -> [SortWarehouse] 
     -> Maybe Int64 -> Maybe Int64 -> Maybe String -> Maybe Int
     -> App [WarehouseStock]
all' session sortWarehouses limit offset filterName filterId = transformAll' <$> findAll' sortWarehouses limit offset filterName filterId

insert' :: MadisonAuthData -> CrudWarehouse -> App Int
insert' showUser crudWarehouse = do
    user     <- getUserByUUID $ suId showUser
    scopedId <- nextScopedId (fromSqlKey $ entityKey user) WarehouseUserId WarehouseScopedId
    new      <- runDb $ insertAndAuditBy ( Warehouse (cwName crudWarehouse) (entityKey user) scopedId )
                                         ( userEmail $ entityVal user )
    case new of
        Left  err -> throwError (err409 { errReasonPhrase = "Duplicate warehouse: " 
                                                            <> (warehouseName $ entityVal err) }) 
        Right key -> return scopedId

update' :: MadisonAuthData -> Int -> CrudWarehouse -> App ()
update' showUser id warehouse = do
    user <- getUserByUUID $ suId showUser
    runDb $ updateWhereAndAudit [WarehouseScopedId ==. id, 
                                 WarehouseUserId   ==. entityKey user] 
                                 [WarehouseName =. cwName warehouse] 
                                 ( userEmail $ entityVal user )

delete' :: MadisonAuthData -> Int -> App ()
delete' showUser id = do
    user <- getUserByUUID $ suId showUser
    runDb $ deleteWhereAndAudit [WarehouseScopedId ==. id, WarehouseUserId ==. entityKey user]
                                ( userEmail $ entityVal user )

