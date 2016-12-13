{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TypeOperators              #-}
module Api.SessionSpec (main, spec) where

import qualified Data.ByteString.Char8            as C
import           Data.ByteString                  (ByteString)
import           Servant                                  (Headers, Header)
import           SpecSupport

import           Api.User
import           Models
import           Api.Session

logIn :: User -> Manager -> BaseUrl -> ClientM (Headers '[Header "madison-auth" ByteString] Api.User.ShowUser)
logIn = client apiSpec

apiSpec :: Proxy Api.Session.API
apiSpec = Proxy

main :: IO ()
main = hspec spec

spec :: Spec
spec = with appSpec $ do
    describe "/login" $ do
        it "Log In" $ do
          warehouse <- liftIO $ createUser "admin@admin.com"
          postJson (C.pack "/login") 
                   (User "admin@admin.com" "123456" Nothing Nothing) `shouldRespondWith` 200


type APISpec = Api.Session.API

serverSpec :: ServerT APISpec App
serverSpec = Api.Session.server

appToServerSpec :: Config -> Server APISpec
appToServerSpec cfg = enter (convertApp cfg) serverSpec

appSpec :: IO Application
appSpec = do
    setupDB
    pool <- makePool Test
    let cfg = Config { getPool = pool, getEnv = Test }
    return $ serveWithContext apiSpec authServerContextSpec (appToServerSpec cfg)
