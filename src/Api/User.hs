{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE DeriveGeneric        #-}

module Api.User where

import           GHC.Generics
import qualified Data.ByteString.Char8       as C
import           Data.Aeson
import           Control.Monad.Except
import           Control.Monad.Reader        (ReaderT, runReaderT)
import           Control.Monad.Reader.Class
import           Data.Int                    (Int64)
import           Database.Persist.Postgresql (entityVal, Entity (..), fromSqlKey, insert,
                                              selectFirst, selectList, (==.))
import           Network.Wai                 (Application)
import           Servant

import           Config                      (App (..), Config (..))
import           Models
import qualified Api.Register                as Register

data ShowUser = ShowUser { suId :: Int64
                         , suEmail :: String } deriving (Show, Read, Generic)

instance ToJSON ShowUser
instance FromJSON ShowUser

type API =
         "users" :> BasicAuth "auth-realm" Register.AuthUser :> Get '[JSON] [ShowUser]
    :<|> "users" :> BasicAuth "auth-realm" Register.AuthUser 
                 :> Capture "email" String                   :> Get '[JSON] ShowUser

server :: ServerT API App
server = allUsers :<|> singleUser

allUsers :: Register.AuthUser -> App [ShowUser]
allUsers user =
    (\user -> (\user' -> ShowUser (fromSqlKey $ entityKey user') 
                                  (userEmail $ entityVal user')) <$> user)
    <$> (runDb $ selectList [] [])

singleUser :: Register.AuthUser -> String -> App ShowUser
singleUser user str = do
    maybeUser <- runDb (selectFirst [UserEmail ==. str] [])
    case maybeUser of
         Nothing ->
            throwError err404
         Just user ->
            return $ ShowUser (fromSqlKey $ entityKey user) (userEmail $ entityVal user)

getUser :: Maybe (Entity User) -> App (Entity User)
getUser Nothing      = throwError err404
getUser (Just user') = return user'

