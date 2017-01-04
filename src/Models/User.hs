{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeFamilies          #-}

module Models.User where

import           Data.Monoid                             ((<>))
import           Control.Monad.Except                    (throwError) 
import           GHC.Generics                            (Generic)
import           Data.Aeson                              (ToJSON, FromJSON)
import           Data.Serialize                          (Serialize)
import           Database.Persist.Postgresql             (entityVal, Entity (..), fromSqlKey, insert,
                                                          selectFirst, selectList, (==.))
import           Servant.Server.Experimental.Auth        (AuthHandler, AuthServerData)
import           Servant.Server.Experimental.Auth.Cookie (AuthCookieData)
import           Servant                                 (AuthProtect, JSON, ServerT, err404)

import           Config
import           Models.Base

data ShowUser = ShowUser { suId    :: String
                         , suEmail :: String } deriving (Show, Read, Generic)

instance ToJSON ShowUser
instance FromJSON ShowUser
instance Serialize ShowUser

type instance AuthServerData (AuthProtect "madison-auth") = ShowUser
type instance AuthCookieData = User

getUser :: Maybe (Entity User) -> App (Entity User)
getUser Nothing      = throwError err404
getUser (Just user') = return user'

showUserBySession :: Maybe (Entity Session) -> App ShowUser
showUserBySession Nothing        = throwError err404
showUserBySession (Just session) = do
        maybeUser <- runDb (selectFirst [UserId ==. sessionUserId (entityVal session)] [])
        case maybeUser of
            Nothing    -> throwError err404
            Just user' -> return $ ShowUser (sessionCookie $ entityVal session) 
                                            (userEmail $ entityVal user')

getUserBySession :: Maybe (Entity Session) -> App (Entity User)
getUserBySession Nothing        = throwError err404
getUserBySession (Just session) = do
        maybeUser <- runDb (selectFirst [UserId ==. sessionUserId (entityVal session)] [])
        case maybeUser of
            Nothing   -> throwError err404
            Just user -> return user

fullName :: User -> String
fullName user = case (userFirstName user <> Just " " <> userLastName user) of
                   Nothing        -> " "
                   Just fullName' -> fullName'
