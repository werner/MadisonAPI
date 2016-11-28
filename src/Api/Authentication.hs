{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric     #-}

module Api.Authentication where

import           Control.Monad.Reader.Class
import           Data.Maybe
import           Servant
import           Control.Monad.Except
import           Control.Monad.Reader             (runReaderT)
import qualified Data.ByteString.Char8            as C
import qualified Data.CaseInsensitive             as CI
import           Servant.Server.Experimental.Auth (AuthHandler, AuthServerData,
                                                   mkAuthHandler)
import           Network.Wai                      (Request, requestHeaders)
import           Servant.Server
import           Database.Persist.Postgresql      (Entity (..), fromSqlKey, insert, runSqlPool,
                                                   selectFirst, selectList, (==.))
import           Control.Monad.IO.Class
import           Data.UUID.V4
import           Data.UUID.Types
import           Config                           (App (..), Config (..), getConfig, convertApp,
                                                   Environment (..), makePool, lookupSetting)
import           Models
import           Api.User

authenticate :: User -> App Api.User.ShowUser
authenticate user = do
    maybeUser <- runDb (selectFirst [UserEmail ==. userEmail user] [])
    case maybeUser of
         Nothing ->
            throwError err404
         Just user -> do
             uuid <- generateUUID user
             return $ Api.User.ShowUser uuid (userEmail $ entityVal user)

generateUUID :: (MonadReader Config App, MonadIO App) => Entity User -> App String
generateUUID user = do
          uuid <- liftIO $ nextRandom
          runDb (insert $ Session (entityKey user) (toString uuid))
          return $ toString uuid
        
authHandler :: AuthHandler Request Api.User.ShowUser
authHandler =
  let handler req = case lookup (CI.mk $ C.pack "madison-auth") (requestHeaders req) of
        Nothing  -> throwError (err401 { errReasonPhrase = "Missing auth header" })
        Just key -> checkSession key
  in mkAuthHandler handler

checkSession :: C.ByteString -> Handler Api.User.ShowUser
checkSession session = do
        cfg <- liftIO $ getConfig
        lookUpUser cfg session

lookUpUser :: Config -> C.ByteString -> Handler Api.User.ShowUser
lookUpUser cfg session = do
  maybeSession <- enter (convertApp cfg) (getSession session)
  enter (convertApp cfg) (getMaybeUserById maybeSession)

getSession :: C.ByteString -> App (Maybe (Entity Session))
getSession session = runDb (selectFirst [SessionCookie ==. C.unpack session] [])

getMaybeUserById :: Maybe (Entity Session) -> App Api.User.ShowUser
getMaybeUserById Nothing        = throwError err404
getMaybeUserById (Just session) = do
        maybeUser <- runDb (selectFirst [UserId ==. (sessionUserId $ entityVal session)] [])
        case maybeUser of
            Nothing    -> throwError err404
            Just user' -> return $ Api.User.ShowUser (sessionCookie $ entityVal session) (userEmail $ entityVal user')