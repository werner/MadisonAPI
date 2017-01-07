
module Api.RegisterSpec (main, spec) where

import qualified Data.ByteString.Char8            as C
import           SpecSupport
import           Data.Monoid                      ((<>))

import           Models.Base
import           Models.Register
import           Api.Register

registerPost    :: RegisterUser -> Manager -> BaseUrl -> ClientM String
confirmationGet :: String -> String -> Manager -> BaseUrl -> ClientM String
registerPost :<|> confirmationGet = client apiSpec

apiSpec :: Proxy Api.Register.API
apiSpec = Proxy

main :: IO ()
main = hspec spec

spec :: Spec
spec = with appSpec $ do
    describe "/register" $ do
        it "Register" $ do
          let user = RegisterUser "test@test.com"
                                  (Just "Test")
                                  (Just "Super Test")
                                  (Just "My Company")
                                  "123456"
                                  "123456"
          postJson (C.pack "/register") user `shouldRespondWith` 200

        it "Confirmates" $ do
          let user = RegisterUser "test@test.com"
                                  (Just "Test")
                                  (Just "Super Test")
                                  (Just "My Company")
                                  "123456"
                                  "123456"
          postJson (C.pack "/register") user `shouldRespondWith` 200
          token <- liftIO $ getTokenByEmail "test@test.com"
          request  (C.pack "GET") (C.pack $ "/confirmation/test@test.com/" <> token) [] (encode "") `shouldRespondWith` 200

type APISpec = Api.Register.API

serverSpec :: ServerT APISpec App
serverSpec = Api.Register.server

appToServerSpec :: Config -> Server APISpec
appToServerSpec cfg = enter (convertApp cfg) serverSpec

appSpec :: IO Application
appSpec = do
    setupDB
    pool <- makePool Test
    let cfg = Config { getPool = pool, getEnv = Test }
    return $ serveWithContext apiSpec authServerContextSpec (appToServerSpec cfg)

