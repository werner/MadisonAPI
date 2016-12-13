{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric     #-}

module Api.Register where

import           Data.Time.Clock
import           Data.Time.Calendar

import           Control.Monad.IO.Class      (liftIO)
import           Control.Exception           (throw)
import           Control.Monad.Except        (throwError)
import           GHC.Generics                (Generic)
import           Data.Aeson                  (ToJSON, FromJSON)
import           Data.Int                    (Int64)
import qualified Data.ByteString.Char8       as C

import           Servant                     ((:>), ReqBody, JSON, Post, ServerT)
import           Servant.Server              (err404)

import           Database.Persist.Postgresql (fromSqlKey, insert, updateWhere, selectFirst, 
                                             (==.), (!=.), (=.))

import           Config                      (App (..), Config (..))
import           Models
import           Api.User
import           Api.Authentication

data RegisterUser = RegisterUser { reEmail                :: String
                                 , rePassword             :: String 
                                 , rePasswordConfirmation :: String} deriving (Show, Read, Generic)

instance ToJSON   RegisterUser
instance FromJSON RegisterUser

type API = "register" :> ReqBody '[JSON] RegisterUser :> Post '[JSON] String

server :: ServerT Api.Register.API App
server = register

register :: RegisterUser -> App String
register user
        | (rePassword user) == (rePasswordConfirmation user) = do
            cryptPasswd <- encryptPassword $ rePassword user
            uuid        <- generateUUID
            date        <- liftIO expirationDate
            user'       <- runDb $ insert $ User (reEmail user) (C.unpack cryptPasswd)
                                                 (Just uuid) (Just date) Nothing Nothing
            return uuid
        | otherwise = throw $ PasswordNotMatch $ "password doesn't match with confirmation"

expirationDate :: IO UTCTime
expirationDate = do
       now <- getCurrentTime
       let twoHours = 2 * 60 * 60
       return $ twoHours `addUTCTime` now

sendConfirmationToken :: String -> App String
sendConfirmationToken email = do
    maybeUser <- runDb (selectFirst [UserEmail ==. email, UserConfirmationToken !=. Nothing] [])
    case maybeUser of
        Nothing   -> throwError err404
        Just user -> do
            uuid  <- generateUUID
            date  <- liftIO expirationDate
            user' <- runDb $ updateWhere [UserEmail ==. email] [UserConfirmationToken =. Just uuid, 
                                                                UserConfirmationTokenExpiration =. Just date]
            return uuid
