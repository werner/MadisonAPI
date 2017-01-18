{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE FlexibleContexts      #-}

module Models.User where

import           Data.Monoid                             ((<>))
import           Control.Monad.Except                    (throwError) 
import           GHC.Generics                            (Generic)
import           Data.Aeson                              (ToJSON, FromJSON)
import           Data.Serialize                          (Serialize, put, get)
import           Data.Text                               (Text, pack)
import           Data.Text.Encoding
import qualified Database.Esqueleto                      as E
import           Database.Persist.Postgresql             (entityVal, Entity (..), fromSqlKey, insert,
                                                          selectFirst, selectList, (==.))
import           Servant.Server.Experimental.Auth        (AuthHandler, AuthServerData)
import           Servant.Server.Experimental.Auth.Cookie (AuthCookieData)
import           Servant                                 (AuthProtect, JSON, ServerT, err404)

import           Config
import           Models.Base

data ShowUser = ShowUser { suId    :: String
                         , suEmail :: Text  } deriving (Show, Read, Generic)

instance Serialize Text where
  put txt = put $ encodeUtf8 txt
  get     = fmap decodeUtf8 get

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

getUserByUUID :: String -> App (Entity User)
getUserByUUID id = do
        user <- runDb
                $ E.select
                $ E.from $ \(users `E.InnerJoin` sessions) -> do
                    E.on $ users E.^. UserId E.==. sessions E.^. SessionUserId
                    return users
        case user of
            (u:_) -> return u
            _     -> throwError err404

fullName :: User -> Text
fullName user = case (userFirstName user <> Just " " <> userLastName user) of
                   Nothing        -> pack " "
                   Just fullName' -> pack fullName'
