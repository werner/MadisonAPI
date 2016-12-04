{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE TypeFamilies          #-}

module Api.User where

import           GHC.Generics                            (Generic)
import qualified Data.ByteString.Char8                   as C
import           Data.Aeson                              (ToJSON, FromJSON)
import           Data.Serialize                          (Serialize)
import           Control.Monad.Except                    (throwError) 
import           Control.Monad.Reader                    (ReaderT, runReaderT)
import           Database.Persist.Postgresql             (entityVal, Entity (..), fromSqlKey, insert,
                                                          selectFirst, selectList, (==.))
import           Network.Wai                             (Application)
import           Servant                                 (AuthProtect, (:>), Get, JSON, ServerT, err404)
import           Servant.Server.Experimental.Auth        (AuthHandler, AuthServerData)
import           Servant.Server.Experimental.Auth.Cookie (AuthCookieData)

import           Api.Types
import           Config                           (App (..), Config (..))
import           Models

data ShowUser = ShowUser { suId    :: String
                         , suEmail :: String } deriving (Show, Read, Generic)

instance ToJSON ShowUser
instance FromJSON ShowUser
instance Serialize ShowUser

type instance AuthServerData (AuthProtect "madison-auth") = ShowUser
type instance AuthCookieData = User

type API = "users" :> MadisonAuthProtect :> Get '[JSON] ShowUser

server :: ServerT API App
server = singleUser

singleUser :: MadisonAuthData -> App ShowUser
singleUser showUser = do
        sessionDB <- runDb (selectFirst [SessionCookie ==. suId showUser] [])
        showUserBySession sessionDB

getUser :: Maybe (Entity User) -> App (Entity User)
getUser Nothing      = throwError err404
getUser (Just user') = return user'

showUserBySession :: Maybe (Entity Session) -> App ShowUser
showUserBySession Nothing        = throwError err404
showUserBySession (Just session) = do
        maybeUser <- runDb (selectFirst [UserId ==. sessionUserId (entityVal session)] [])
        case maybeUser of
            Nothing    -> throwError err404
            Just user' -> return $ Api.User.ShowUser (sessionCookie $ entityVal session) 
                                                     (userEmail $ entityVal user')

getUserBySession :: Maybe (Entity Session) -> App (Entity User)
getUserBySession Nothing        = throwError err404
getUserBySession (Just session) = do
        maybeUser <- runDb (selectFirst [UserId ==. sessionUserId (entityVal session)] [])
        case maybeUser of
            Nothing   -> throwError err404
            Just user -> return user
