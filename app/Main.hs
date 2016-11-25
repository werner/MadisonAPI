module Main where

import           Database.Persist.Postgresql (runSqlPool)
import           Network.Wai.Handler.Warp    (run)
import           System.Environment          (lookupEnv)

import           Api                         (app)
import           Api.User                    
import qualified Api.Warehouse               as WarehouseApi
import           Config                      (Config (..), Environment (..), getConfig, getPort,
                                              makePool, setLogger, lookupSetting)
import           Models                      (doMigrations)
import           Safe                        (readMay)

main :: IO ()
main = do
    cfg  <- getConfig
    port <- getPort
    let logger = setLogger $ getEnv cfg
    runSqlPool doMigrations $ getPool cfg
    run port $ logger $ app cfg
