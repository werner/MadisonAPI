{-# LANGUAGE QuasiQuotes                #-}
module Api.WarehouseSpec (main, spec) where

import qualified Data.ByteString.Char8      as C
import           Data.ByteString            (ByteString)
import           Data.CaseInsensitive       as CI

import           Test.Hspec
import           Test.Hspec.Wai
import           Test.Hspec.Wai.JSON
import           Data.Aeson                  (Value(..), object, (.=), ToJSON, encode)
import           Network.HTTP.Client         (Manager, newManager, defaultManagerSettings)
import           Network.HTTP.Types
import           Network.Wai                 (Application)
import           Network.Wai.Handler.Warp
import           Network.Wai.Test            (SResponse)
import           Servant
import           Servant.Client
import qualified Database.Persist.Postgresql as P
import           Data.Text                   (Text)
import           Data.Int                    (Int64)
import           Control.Monad.Reader        (MonadIO, MonadReader, ReaderT, ask, runReaderT)
import           Control.Monad.Except        (ExceptT, MonadError, runExceptT)

import           Config                      (App, runApp, Config (..), Environment (..), makePool, setLogger)
import           Models
import           Api
import qualified Api.User                     as ApiUser
import           Api.Warehouse

getAll       :: Maybe String -> Maybe SortOrder -> Maybe Int64 -> Maybe Int64 -> Manager -> BaseUrl -> ClientM [WarehouseStock]
getShow      :: Int64 -> Manager -> BaseUrl -> ClientM (P.Entity Warehouse)
postInsert   :: CrudWarehouse -> Manager -> BaseUrl -> ClientM Int64
putUpdate    :: Int64 -> CrudWarehouse -> Manager -> BaseUrl -> ClientM Int64
deleteDelete :: Int64 -> Manager -> BaseUrl -> ClientM Int64
getAll :<|> getShow :<|> postInsert :<|> putUpdate :<|> deleteDelete = client apiSpec

apiSpec :: Proxy Api.Warehouse.API
apiSpec = Proxy

main :: IO ()
main = hspec spec

spec :: Spec
spec = with appSpec $ do
    describe "/warehouses" $ do
        it "list warehouses" $ do
          get (C.pack "/warehouses") `shouldRespondWith` [json|[]|]


serverSpec :: ServerT Api.Warehouse.API App
serverSpec = Api.Warehouse.server

appToServerSpec :: Config -> Server Api.Warehouse.API
appToServerSpec cfg = enter (convertApp cfg) serverSpec

appSpec :: IO Application
appSpec = do
    pool <- makePool Test
    let cfg = Config { getPool = pool, getEnv = Test }
    P.runSqlPool doMigrations pool
    return $ serve apiSpec (appToServerSpec cfg)

postJson :: (ToJSON a) => ByteString -> a -> WaiSession SResponse
postJson path =
    request methodPost path [(CI.mk (C.pack "Content-Type"), (C.pack "application/json"))] . encode

createUser :: App Int64
createUser = do
        let user = User "test@test.com" "12345" Nothing Nothing
        id <- ApiUser.createUser user
        return id 

builtWarehouse :: App WarehouseStock
builtWarehouse = do
        userId' <- createUser
        return $ WarehouseStock 0 "Second" userId' 0.0 

getWarehouseFromStock :: App WarehouseStock -> Config -> (ExceptT ServantErr IO) WarehouseStock
getWarehouseFromStock = runReaderT . runApp
