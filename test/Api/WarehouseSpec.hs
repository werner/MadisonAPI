{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TypeOperators              #-}
module Api.WarehouseSpec (main, spec) where

import qualified Data.ByteString.Char8            as C
import           Data.ByteString                  (ByteString)
import           Data.CaseInsensitive             as CI
import           Data.Text                        as T

import           Api.Warehouse
import           Models.Base
import           Models.Warehouse

import           SpecSupport

type MadisonAuthReq = AuthenticateReq MadisonAuthProtect

getAll       :: MadisonAuthReq -> [SortWarehouse] 
                               -> Maybe Int64 
                               -> Maybe Int64 
                               -> Maybe String
                               -> Maybe Int 
                               -> Manager 
                               -> BaseUrl 
                               -> ClientM [WarehouseStock]
postInsert   :: MadisonAuthReq -> CrudWarehouse -> Manager -> BaseUrl -> ClientM Int
putUpdate    :: MadisonAuthReq -> Int -> CrudWarehouse -> Manager -> BaseUrl -> ClientM ()
deleteDelete :: MadisonAuthReq -> Int -> Manager -> BaseUrl -> ClientM ()
getAll :<|> postInsert :<|> putUpdate :<|> deleteDelete = client apiSpec

apiSpec :: Proxy Api.Warehouse.API
apiSpec = Proxy

main :: IO ()
main = hspec spec

spec :: Spec
spec = with appSpec $ do
    describe "/warehouses" $ do
        it "list warehouses" $ do
          liftIO $ createWarehouse "First"
          liftIO $ createWarehouse "Second"
          liftIO $ createWarehouse "Third"
          (request (C.pack "GET")  (C.pack "/warehouses?sortField=name-asc&sortField=id-asc&limit=10&offset=0") 
                  [(CI.mk (C.pack "Content-Type"), (C.pack "application/json")),
                   (CI.mk (C.pack "madison-auth"), (C.pack "key-test"))] $ encode "") `shouldRespondWith` 200  

        it "creates a warehouse" $ do
          postJson (C.pack "/warehouses") (CrudWarehouse "Second") `shouldRespondWith` [json|2|]

        it "Throw a 409 error status code on a duplicate warehouse" $ do
          liftIO $ createWarehouse "Second"
          postJson (C.pack "/warehouses") (CrudWarehouse "Second") `shouldRespondWith` 409

        it "updates a warehouse" $ do
          warehouse <- liftIO $ createWarehouse "Second"
          let id    = warehouseScopedId $ entityVal warehouse
          let path  = "/warehouses/" ++ (show id)
          putJson (C.pack path) (CrudWarehouse "Third") `shouldRespondWith` 200

        it "deletes a warehouse" $ do
          warehouse <- liftIO $ createWarehouse "first"
          let id    = warehouseScopedId $ entityVal warehouse
          let path  = "/warehouses/" ++ (show id)
          deleteJson (C.pack path) `shouldRespondWith` 200

type APISpec = Api.Warehouse.API

serverSpec :: ServerT APISpec App
serverSpec = Api.Warehouse.server

appToServerSpec :: Config -> Server APISpec
appToServerSpec cfg = enter (convertApp cfg) serverSpec

appSpec :: IO Application
appSpec = do
    setupDB
    pool <- makePool Test
    let cfg = Config { getPool = pool, getEnv = Test }
    return $ serveWithContext apiSpec authServerContextSpec (appToServerSpec cfg)

createWarehouse :: String -> IO (Entity Warehouse)
createWarehouse name = do
        pool <- makePool Test
        user <- runSqlPool (selectFirst [UserEmail ==. T.pack "logged_user@user.com"] []) pool
        runSqlPool (insert $ Warehouse name (entityKey $ getUserFromMaybe user) 1) pool
        warehouse <- runSqlPool (selectFirst [WarehouseName ==. name] []) pool
        case warehouse of
            Just w  -> return $ w
            Nothing -> throw err404

getUserFromMaybe :: (Maybe (Entity User)) -> (Entity User)
getUserFromMaybe (Just user) = user
getUserFromMaybe Nothing     = throw err404
