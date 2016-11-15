{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators              #-}

module Api.User where

import           Control.Monad.Except
import           Control.Monad.Reader        (ReaderT, runReaderT)
import           Control.Monad.Reader.Class
import           Data.Int                    (Int64)
import           Database.Persist.Postgresql (Entity (..), fromSqlKey, insert,
                                              selectFirst, selectList, (==.))
import           Network.Wai                 (Application)
import           Servant

import           Config                      (App (..), Config (..))
import           Models

type UserAPI =
         "users" :> Get '[JSON] [Entity User]
    :<|> "users" :> Capture "name" String :> Get '[JSON] (Entity User)
    :<|> "users" :> ReqBody '[JSON] User :> Post '[JSON] Int64

userServer :: ServerT UserAPI App
userServer = allUsers :<|> singleUser :<|> createUser

allUsers :: App [Entity User]
allUsers =
    runDb (selectList [] [])

singleUser :: String -> App (Entity User)
singleUser str = do
    maybeUser <- runDb (selectFirst [UserEmail ==. str] [])
    case maybeUser of
         Nothing ->
            throwError err404
         Just person ->
            return person

createUser :: User -> App Int64
createUser p = do
    newUser <- runDb $ insert $ User (userEmail p) (userPassword p) Nothing Nothing
    return $ fromSqlKey newUser
