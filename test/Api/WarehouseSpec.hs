{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TypeOperators              #-}
module Api.WarehouseSpec (main, spec) where

import           Debug.Trace
import qualified Data.ByteString.Char8            as C
import           Data.ByteString                  (ByteString)
import           Data.CaseInsensitive             as CI

import           Control.Exception
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
import           Control.Monad.Reader              (MonadIO, MonadReader, ReaderT, ask, runReaderT)
import           Control.Monad.Except              (ExceptT, MonadError, runExceptT)
import           Control.Monad

import           Config                            (App, runApp, Config (..), Environment (..), 
                                                    convertApp, makePool, setLogger)
import           Models
import           Api
import           Api.Types
import           Api.Authentication
import           Api.User 
import           Api.Warehouse
import           Api.Register

import           SpecSupport

type MadisonAuthReq = AuthenticateReq MadisonAuthProtect

getAll       :: MadisonAuthReq -> Maybe String -> Maybe SortOrder -> Maybe Int64 -> Maybe Int64 -> Manager -> BaseUrl -> ClientM [WarehouseStock]
getShow      :: MadisonAuthReq -> Int64 -> Manager -> BaseUrl -> ClientM (P.Entity Warehouse)
postInsert   :: MadisonAuthReq -> CrudWarehouse -> Manager -> BaseUrl -> ClientM Int64
putUpdate    :: MadisonAuthReq -> Int64 -> CrudWarehouse -> Manager -> BaseUrl -> ClientM Int64
deleteDelete :: MadisonAuthReq -> Int64 -> Manager -> BaseUrl -> ClientM Int64
getAll :<|> getShow :<|> postInsert :<|> putUpdate :<|> deleteDelete = client apiSpec

apiSpec :: Proxy Api.Warehouse.API
apiSpec = Proxy

main :: IO ()
main = hspec spec

spec :: Spec
spec = with appSpec $ do
    describe "/warehouses" $ do
        it "list warehouses" $ do
          request (C.pack "GET")  (C.pack "/warehouses") 
                  [(CI.mk (C.pack "madison-auth"),  (C.pack "key-test"))] 
                  (encode "") `shouldRespondWith` [json|[]|]  

        it "creates a warehouse" $ do
          postJson (C.pack "/warehouses") (CrudWarehouse "Second") `shouldRespondWith` 200

        it "Throw a 409 error status code on a duplicate warehouse" $ do
          liftIO $ getIOWarehouse
          postJson (C.pack "/warehouses") (CrudWarehouse "Second") `shouldRespondWith` 409

        it "updates a warehouse" $ do
          warehouse <- liftIO $ getIOWarehouse
          let id    = P.fromSqlKey $ P.entityKey warehouse
          let path  = "/warehouses/" ++ (show id)
          putJson (C.pack path) (CrudWarehouse "Third") `shouldRespondWith` 200

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

createUser :: String -> IO Api.User.ShowUser
createUser email = do
        pool <- makePool Test
        user' <- P.runSqlPool (P.insert $ User email "12345" Nothing Nothing) pool
        return $ Api.User.ShowUser "1234" email

getIOWarehouse :: IO (P.Entity Warehouse)
getIOWarehouse = do
        pool <- makePool Test
        user <- P.runSqlPool (P.selectFirst [UserEmail P.==. "logged_user@user.com"] []) pool
        P.runSqlPool (P.insert $ Warehouse "Second" (P.entityKey $ getUserFromMaybe user) Nothing Nothing) pool
        warehouse <- P.runSqlPool (P.selectFirst [WarehouseName P.==. "Second"] []) pool
        case warehouse of
            Just w  -> return $ w
            Nothing -> throw err404

getUserFromMaybe :: (Maybe (P.Entity User)) -> (P.Entity User)
getUserFromMaybe (Just user) = user
getUserFromMaybe Nothing     = throw err404
