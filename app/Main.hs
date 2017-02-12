module Main where

import           Data.Monoid                 ((<>))
import           Network.Wai.Middleware.Cors
import qualified Data.ByteString.Char8       as C
import qualified Data.CaseInsensitive        as CI
import           Network.Wai                 (Application)
import           Database.Persist.Postgresql (runSqlPool)
import           Network.Wai.Handler.Warp    (run)
import           System.Environment          (lookupEnv)

import           Api                         (app)
import           Api.User                    
import qualified Api.Warehouse               as WarehouseApi
import           Config                      (Config (..), Environment (..), getConfig, getPort,
                                              makePool, setLogger, lookupSetting)
import           Models.Base                 (doMigrations)
import           Safe                        (readMay)


main :: IO ()
main = do
    cfg  <- getConfig
    port <- getPort
    let logger = setLogger $ getEnv cfg
    runSqlPool doMigrations $ getPool cfg
    run port $ logger $ stack $ app cfg

stack :: Application -> Application
stack = cors'
  where
    cors' = cors (const $ Just corsResourcePolicy)

corsResourcePolicy :: CorsResourcePolicy
corsResourcePolicy = CorsResourcePolicy
  { corsOrigins        = Nothing
  , corsMethods        = [C.pack "GET",  C.pack "HEAD", C.pack "OPTIONS", 
                          C.pack "POST", C.pack "PUT",  C.pack "DELETE"]
  , corsRequestHeaders = simpleResponseHeaders <> [CI.mk $ C.pack "madison-auth"]
  , corsExposedHeaders = Nothing
  , corsMaxAge         = Nothing
  , corsVaryOrigin     = False
  , corsRequireOrigin  = False
  , corsIgnoreFailures = False
  }
