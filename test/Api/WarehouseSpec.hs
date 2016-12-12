{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TypeOperators              #-}
module Api.WarehouseSpec (main, spec) where

import qualified Data.ByteString.Char8            as C
import           Data.ByteString                  (ByteString)
import           Data.CaseInsensitive             as CI

import           Test.Hspec
import           Test.Hspec.Wai
import           Test.Hspec.Wai.JSON
import           Data.Aeson                       (Value(..), object, (.=), ToJSON, encode)
import           Network.HTTP.Client              (Manager, newManager, defaultManagerSettings)
import           Network.HTTP.Types
import           Network.Wai                      (Application, Request)
import           Network.Wai.Handler.Warp
import           Network.Wai.Test                 (SResponse)
import           Servant
import           Servant.Server                   (BasicAuthCheck (BasicAuthCheck), 
                                                   serveWithContext, Context, Context ((:.), EmptyContext))

import           Servant.Server.Experimental.Auth (AuthHandler)
import           Servant.Client
import qualified Database.Persist.Postgresql       as P
import           Data.Text                         (Text)
import           Data.Int                          (Int64)

import           Models
import           Api
import           Api.Types
import           Api.Authentication
import           Api.User 
import           Api.Warehouse
import           Api.Register

import           SpecSupport

type MadisonAuthReq = AuthenticateReq MadisonAuthProtect

getAll       :: MadisonAuthReq -> [SortWarehouse] -> Maybe Int64 -> Maybe Int64 -> FilterWarehouse -> Manager -> BaseUrl -> ClientM [WarehouseStock]
postInsert   :: MadisonAuthReq -> CrudWarehouse -> Manager -> BaseUrl -> ClientM Int
putUpdate    :: MadisonAuthReq -> Int -> CrudWarehouse -> Manager -> BaseUrl -> ClientM Int
deleteDelete :: MadisonAuthReq -> Int -> Manager -> BaseUrl -> ClientM Int
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
          request (C.pack "GET")  (C.pack "/warehouses?sortField=SWarehouseNameAsc&sortField=SWarehouseScopedIdAsc&limit=10&offset=0") 
                  [(CI.mk (C.pack "Content-Type"), (C.pack "application/json")),
                   (CI.mk (C.pack "madison-auth"),  (C.pack "key-test"))] 
                  (encode $ FilterWarehouse Nothing Nothing) `shouldRespondWith` 200  

        it "creates a warehouse" $ do
          postJson (C.pack "/warehouses") (CrudWarehouse "Second") `shouldRespondWith` [json|2|]

        it "Throw a 409 error status code on a duplicate warehouse" $ do
          liftIO $ createWarehouse "Second"
          postJson (C.pack "/warehouses") (CrudWarehouse "Second") `shouldRespondWith` 409

        it "updates a warehouse" $ do
          warehouse <- liftIO $ createWarehouse "Second"
          let id    = P.fromSqlKey $ P.entityKey warehouse
          let path  = "/warehouses/" ++ (show id)
          putJson (C.pack path) (CrudWarehouse "Third") `shouldRespondWith` 200

        it "deletes a warehouse" $ do
          warehouse <- liftIO $ createWarehouse "first"
          let id    = P.fromSqlKey $ P.entityKey warehouse
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

createWarehouse :: String -> IO (P.Entity Warehouse)
createWarehouse name = do
        pool <- makePool Test
        user <- P.runSqlPool (P.selectFirst [UserEmail P.==. "logged_user@user.com"] []) pool
        P.runSqlPool (P.insert $ Warehouse name (P.entityKey $ getUserFromMaybe user) 1 Nothing Nothing) pool
        warehouse <- P.runSqlPool (P.selectFirst [WarehouseName P.==. name] []) pool
        case warehouse of
            Just w  -> return $ w
            Nothing -> throw err404

getUserFromMaybe :: (Maybe (P.Entity User)) -> (P.Entity User)
getUserFromMaybe (Just user) = user
getUserFromMaybe Nothing     = throw err404
