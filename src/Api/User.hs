{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE TypeFamilies          #-}

module Api.User where

import           GHC.Generics
import qualified Data.ByteString.Char8            as C
import           Data.Aeson
import           Data.Serialize                   (Serialize)
import           Control.Monad.Except
import           Control.Monad.Reader             (ReaderT, runReaderT)
import           Control.Monad.Reader.Class
import           Control.Monad.Trans.Either
import           Data.Int                         (Int64)
import           Database.Persist.Postgresql      (entityVal, Entity (..), fromSqlKey, insert,
                                                   selectFirst, selectList, (==.))
import           Network.Wai                      (Application)
import           Servant
import           Servant.Server.Experimental.Auth (AuthHandler)
import           Servant.Server.Experimental.Auth.Cookie

import           Api.Types
import           Config                           (App (..), Config (..))
import           Models

data ShowUser = ShowUser { suId    :: String
                         , suEmail :: String } deriving (Show, Read, Generic)

instance ToJSON ShowUser
instance FromJSON ShowUser
instance Serialize Api.User.ShowUser

type instance AuthCookieData = User

type API =
         "users" :> MadisonAuthProtect     :> Get '[JSON] [Entity User]
    :<|> "users" :> MadisonAuthProtect 
                 :> Capture "email" String :> Get '[JSON] String

server :: ServerT API App
server = allUsers :<|> singleUser

allUsers :: MadisonAuthData -> App [Entity User]
allUsers session =
    runDb $ selectList [] []

singleUser :: MadisonAuthData -> String -> App String
singleUser session str = do
    maybeUser <- runDb (selectFirst [UserEmail ==. str] [])
    case maybeUser of
         Nothing ->
            throwError err404
         Just user ->
            return $ userEmail $ entityVal user

getUser :: Maybe (Entity User) -> App (Entity User)
getUser Nothing      = throwError err404
getUser (Just user') = return user'
