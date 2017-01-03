{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}

module Lib.Authentication where

import           Control.Exception                (Exception, throw)
import           GHC.Generics                     (Generic)
import           Data.Aeson                       (ToJSON, FromJSON)
import           Data.Typeable                    (Typeable)
import           Data.Maybe                       (fromMaybe)
import           Crypto.BCrypt                    (hashPasswordUsingPolicy, fastBcryptHashingPolicy
                                                  , validatePassword)

import           Control.Monad.Reader.Class       (MonadReader)
import           Control.Monad.Except             (throwError)
import           Control.Monad.Reader             (runReaderT)
import qualified Data.ByteString.Char8            as C
import qualified Data.CaseInsensitive             as CI
import           Servant.Server.Experimental.Auth (AuthHandler, AuthServerData,
                                                   mkAuthHandler)
import           Network.Wai                      (Request, requestHeaders)
import           Servant.Server                   (errReasonPhrase, Handler, enter, err401, err404, err500)
import           Database.Persist.Postgresql      (Entity (..), fromSqlKey, insert, runSqlPool, delete,
                                                   selectFirst, selectList, (==.))
import           Control.Monad.IO.Class           (MonadIO, liftIO)
import           Data.UUID.V4                     (nextRandom)
import           Data.UUID.Types                  (toString)
import           Config                           (App (..), Config (..), getConfig, convertApp,
                                                   Environment (..), makePool, lookupSetting)
import           Models.Base
import           Api.User

data AuthUser = AuthUser { authEmail    :: String
                         , authPassword :: String }
                         deriving (Show, Read, Generic)

instance ToJSON    AuthUser
instance FromJSON  AuthUser

data AuthenticationException = PasswordNotMatch String
                        deriving (Show, Typeable)

instance Exception AuthenticationException

authenticate :: AuthUser -> App Api.User.ShowUser
authenticate au = do
    let password = authPassword au
    maybeUser <- runDb (selectFirst [UserEmail                       ==. authEmail au, 
                                     UserConfirmationToken           ==. Nothing,
                                     UserConfirmationTokenExpiration ==. Nothing] [])
    case maybeUser of
         Nothing   -> throwError err404
         Just user -> do
             case validatePassword (C.pack $ userPassword $ entityVal user) (C.pack password) of
                 True -> do
                     uuid    <- generateUUID
                     session <- runDb (selectFirst [SessionUserId ==. entityKey user] [])
                     case session of
                         Just s  -> do
                             runDb $ delete $ entityKey s
                             runDb $ insert $ Session (entityKey user) uuid
                         Nothing -> runDb $ insert $ Session (entityKey user) uuid
                     return $ Api.User.ShowUser uuid (userEmail $ entityVal user)
                 False -> throwError (err500 { errReasonPhrase = "password doesn't match" })

generateUUID :: (MonadReader Config App, MonadIO App) => App String
generateUUID = do
          uuid <- liftIO nextRandom
          return $ toString uuid
        
authHandler :: AuthHandler Request Api.User.ShowUser
authHandler =
  let handler req = case lookup (CI.mk $ C.pack "madison-auth") (requestHeaders req) of
        Nothing  -> throwError (err401 { errReasonPhrase = "Missing auth header" })
        Just key -> checkSession key
  in mkAuthHandler handler

checkSession :: C.ByteString -> Handler Api.User.ShowUser
checkSession session = do
        cfg <- liftIO getConfig
        lookUpUser cfg session

lookUpUser :: Config -> C.ByteString -> Handler Api.User.ShowUser
lookUpUser cfg session = do
  maybeSession <- enter (convertApp cfg) (getSession session)
  enter (convertApp cfg) (Api.User.showUserBySession maybeSession)

getSession :: C.ByteString -> App (Maybe (Entity Session))
getSession session = runDb (selectFirst [SessionCookie ==. C.unpack session] [])

encryptPassword :: String -> App C.ByteString
encryptPassword password = liftIO $ (fromMaybe $ C.pack "") <$> 
                                    (hashPasswordUsingPolicy fastBcryptHashingPolicy $ C.pack password)
