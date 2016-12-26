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

import           Servant                     ((:<|>)(..), (:>), ReqBody, JSON, Post, Get, ServerT, Capture)
import           Servant.Server              (err404)

import           Database.Persist.Postgresql (fromSqlKey, insert, updateWhere, selectFirst, entityVal,
                                             (==.), (!=.), (=.))

import           Config                      (App (..), Config (..), getHost)
import           Models
import           Api.User
import           Lib.Mail
import           Lib.Authentication

data RegisterUser = RegisterUser { reEmail                :: String
                                 , reFirstName            :: Maybe String
                                 , reLastName             :: Maybe String
                                 , reCompanyName          :: Maybe String
                                 , rePassword             :: String 
                                 , rePasswordConfirmation :: String} deriving (Show, Read, Generic)

instance ToJSON   RegisterUser
instance FromJSON RegisterUser

type API =  "register"     :> ReqBody '[JSON] RegisterUser :> Post '[JSON] String
       :<|> "confirmation" :> Capture "email" String       
                           :> Capture "token" String       :> Get  '[JSON] String

server :: ServerT Api.Register.API App
server = register :<|> confirmation

register :: RegisterUser -> App String
register user
        | (rePassword user) == (rePasswordConfirmation user) = do
            cryptPasswd <- encryptPassword $ rePassword user
            uuid        <- generateUUID
            date        <- liftIO expirationDate
            user'       <- runDb $ insert $ User (reEmail user) (C.unpack cryptPasswd)
                                                 (reFirstName user) (reLastName user) Nothing
                                                 (Just uuid) (Just date) Nothing Nothing
            sendConfirmationToken $ reEmail user
            return uuid
        | otherwise = throw $ PasswordNotMatch $ "password doesn't match with confirmation"

confirmation :: String -> String -> App String
confirmation email token = do
    maybeUser <- runDb (selectFirst [UserEmail ==. email, UserConfirmationToken ==. Just token] [])
    case maybeUser of
        Nothing   -> throwError err404
        Just user -> do 
            runDb $ updateWhere [UserEmail ==. email] [UserConfirmationToken           =. Nothing, 
                                                       UserConfirmationTokenExpiration =. Nothing]
            return "Confirmation Successful"

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

            liftIO $ sendEmail (ToEmail (fullName $ entityVal user) email)  "Confirmation Email" 
                               (bodyTextConfirmationEmail uuid) (bodyHtmlConfirmationEmail uuid)
            return uuid

bodyTextConfirmationEmail :: String -> String
bodyTextConfirmationEmail uuid = 
        "Thanks for registering, please got to " ++ getHost ++ uuid ++ " to confirm your subscription."

bodyHtmlConfirmationEmail :: String -> String
bodyHtmlConfirmationEmail uuid = 
        "Thanks for registering, please got to <a href='" ++ getHost ++ uuid ++ "'>" ++
        getHost ++ uuid ++ "</a> to confirm your subscription."
