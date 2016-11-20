{-# LANGUAGE QuasiQuotes                #-}
module Api.WarehouseSpec (main, spec) where

import qualified Data.ByteString.Char8      as C

import           Test.Hspec
import           Test.Hspec.Wai
import           Test.Hspec.Wai.JSON
import           Data.Aeson                  (Value(..), object, (.=))
import           Network.HTTP.Client         (Manager, newManager, defaultManagerSettings)
import           Network.HTTP.Types
import           Network.Wai                 (Application)
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.Client
import qualified Database.Persist.Postgresql as P
import           Data.Text                   (Text)
import           Data.Int                    (Int64)

import           Config                      (App, Config (..), Environment (..), makePool, setLogger)
import           Models
import           Api
import           Api.Warehouse

getAll       :: Maybe String -> Maybe SortOrder -> Maybe Int64 -> Maybe Int64 -> Manager -> BaseUrl -> ClientM [WarehouseStock]
getShow      :: Int64 -> Manager -> BaseUrl -> ClientM (P.Entity Warehouse)
postInsert   :: Warehouse -> Manager -> BaseUrl -> ClientM Int64
putUpdate    :: Int64 -> Warehouse -> Manager -> BaseUrl -> ClientM Int64
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
    return $ serve apiSpec (appToServerSpec cfg)
