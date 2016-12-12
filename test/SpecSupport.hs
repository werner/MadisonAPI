{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE TypeOperators        #-}

module SpecSupport ( module Debug.Trace 
                   , module Control.Exception
                   , module Control.Monad
                   , ExceptT, MonadError, runExceptT, MonadIO, MonadReader, ReaderT, ask, runReaderT
                   , authServerContextSpec, deleteJson, putJson, postJson, setupDB, createUser) where


import           Control.Monad
import           Control.Exception
import           Debug.Trace
import           Data.List                         as L
import           Data.Text                         as T
import qualified Data.ByteString.Char8             as C
import           Data.ByteString                   (ByteString)
import           Data.CaseInsensitive              as CI
import           Database.Persist.Types
import           Control.Monad.Reader              (MonadIO, MonadReader, ReaderT, ask, runReaderT)
import           Database.Persist.Sql              (SqlPersistM, SqlBackend, runSqlPersistMPool, rawExecute, 
                                                    rawSql, unSingle, connEscapeName)

import           Control.Monad.Reader              (MonadIO, MonadReader, ReaderT, ask, runReaderT)
import           Control.Monad.Except              (ExceptT, MonadError, runExceptT)
import           Test.Hspec
import           Test.Hspec.Wai
import           Test.Hspec.Wai.JSON
import           Data.Aeson                        (Value(..), object, (.=), ToJSON, encode)
import           Network.HTTP.Types
import           Network.Wai                       (Application, Request)
import           Network.Wai.Test                  (SResponse)
import qualified Database.Persist.Postgresql       as P
import           Servant.Server                    (BasicAuthCheck (BasicAuthCheck), 
                                                    serveWithContext, Context, Context ((:.), EmptyContext))
import           Servant.Server.Experimental.Auth  (AuthHandler)

import           Config
import           Models
import           Debug.Trace
import           Api.Authentication
import           Api.User 

wipeDB :: ReaderT SqlBackend IO ()
wipeDB = do
    tables     <- getTables
    sqlBackend <- ask

    let escapedTables = Prelude.map (connEscapeName sqlBackend . DBName) tables
        query = T.append (T.pack "TRUNCATE TABLE ")  (T.intercalate (T.pack ", ") escapedTables)

    rawExecute query  []


getTables :: MonadIO IO => ReaderT SqlBackend IO [Text]
getTables = do
    tables <- rawSql (T.pack "SELECT table_name FROM information_schema.tables WHERE table_schema = 'public'") []
    return $ Prelude.map unSingle tables

setupDB :: IO (Key Session)
setupDB = do
    pool <- makePool Test
    P.runSqlPool doMigrations pool
    P.runSqlPool wipeDB pool
    loggedIn <- P.runSqlPool (P.insert $ User "logged_user@user.com" "12345" Nothing Nothing) pool 
    P.runSqlPool (P.insert $ Session loggedIn "key-test") pool 

postJson :: (ToJSON a) => ByteString -> a -> WaiSession SResponse
postJson path =
    request methodPost path [(CI.mk (C.pack "Content-Type"), (C.pack "application/json")), 
                             (CI.mk (C.pack "madison-auth"), (C.pack "key-test"))] . encode

putJson :: (ToJSON a) => ByteString -> a -> WaiSession SResponse
putJson path =
    request methodPut path [(CI.mk (C.pack "Content-Type"), (C.pack "application/json")), 
                             (CI.mk (C.pack "madison-auth"), (C.pack "key-test"))] . encode

deleteJson :: ByteString -> WaiSession SResponse
deleteJson path =
    request methodDelete path [(CI.mk (C.pack "Content-Type"), (C.pack "application/json")), 
                               (CI.mk (C.pack "madison-auth"), (C.pack "key-test"))] $ encode ""

authServerContextSpec :: Context (AuthHandler Request Api.User.ShowUser ': '[])
authServerContextSpec = authHandler :. EmptyContext

createUser :: String -> IO Api.User.ShowUser
createUser email = do
        pool <- makePool Test
        user' <- P.runSqlPool (P.insert $ User email "12345" Nothing Nothing) pool
        return $ Api.User.ShowUser "1234" email

