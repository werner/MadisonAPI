{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE TypeOperators        #-}

module SpecSupport ( module Debug.Trace 
                   , module Control.Exception
                   , module Control.Monad
                   , module Data.Maybe
                   , module Servant.API.Alternative
                   , module Models
                   , module Api.Types
                   , ExceptT, MonadError, runExceptT, MonadIO, MonadReader, ReaderT, ask, runReaderT
                   , App, runApp, Config (..), Environment (..), convertApp, makePool, setLogger
                   , Value(..), object, (.=), ToJSON, encode, Int64
                   , serveWithContext, Context ((:.), EmptyContext)
                   , Spec, hspec, describe, it
                   , shouldRespondWith, request, with, liftIO
                   , json
                   , fromSqlKey, runSqlPool, selectFirst, (==.), insert, Entity(..)
                   , Server, ServerT, Proxy(..), enter, err404
                   , client, ClientM, BaseUrl, AuthenticateReq
                   , Manager, newManager, defaultManagerSettings
                   , Application, Request
                   , authServerContextSpec, deleteJson, putJson, postJson, setupDB, createUser) where

import           Control.Monad
import           Data.Maybe
import           Control.Exception
import           Debug.Trace
import           Data.Int                          (Int64)
import           Data.Text                         as T
import qualified Data.ByteString.Char8             as C
import           Data.ByteString                   (ByteString)
import           Data.CaseInsensitive              as CI
import           Database.Persist.Types
import           Control.Monad.Reader              (MonadIO, MonadReader, ReaderT, ask, runReaderT)
import           Database.Persist.Sql              (SqlPersistM, SqlBackend, runSqlPersistMPool, rawExecute, 
                                                    rawSql, unSingle, connEscapeName)

import           Data.Aeson                        (Value(..), object, (.=), ToJSON, encode)
import           Control.Monad.Reader              (MonadIO, MonadReader, ReaderT, ask, runReaderT)
import           Control.Monad.Except              (ExceptT, MonadError, runExceptT)
import           Test.Hspec                        (Spec, hspec, describe, it)
import           Test.Hspec.Wai                    (WaiSession, shouldRespondWith, request, with, liftIO)
import           Test.Hspec.Wai.JSON               (json)
import           Network.HTTP.Types
import           Network.Wai                       (Application, Request)
import           Network.Wai.Test                  (SResponse)
import           Database.Persist.Postgresql       (fromSqlKey, entityKey, runSqlPool, selectFirst, (==.)
                                                   , insert, Entity(..))
import           Servant.Server                    (BasicAuthCheck (BasicAuthCheck), 
                                                    serveWithContext, Context, Context ((:.), EmptyContext))
import           Servant.Server.Experimental.Auth  (AuthHandler)
import           Servant                           (Server, ServerT, Proxy(..), enter, err404)
import           Servant.Client                    (client, ClientM, BaseUrl, AuthenticateReq)
import           Servant.API.Alternative
import           Network.HTTP.Client               (Manager, newManager, defaultManagerSettings)
import           Network.Wai                       (Application, Request)
import           Crypto.BCrypt                     (hashPasswordUsingPolicy, fastBcryptHashingPolicy
                                                   , validatePassword)


import           Config                            (App, runApp, Config(..), Environment(..), 
                                                    convertApp, makePool, setLogger)
import           Models
import           Api.Types
import           Api.User 
import           Lib.Authentication

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
    runSqlPool doMigrations pool
    runSqlPool wipeDB pool
    loggedIn <- createUser  "logged_user@user.com"
    runSqlPool (insert $ Session loggedIn "key-test") pool 

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

createUser :: String -> IO (Key User)
createUser email = do
        pool <- makePool Test
        let encryptPassword password = (fromMaybe $ C.pack "") <$> 
                                        (hashPasswordUsingPolicy fastBcryptHashingPolicy $ C.pack password)
        cryptPasswd <- encryptPassword "123456"
        runSqlPool (insert $ User email (C.unpack cryptPasswd) Nothing Nothing Nothing Nothing) pool
