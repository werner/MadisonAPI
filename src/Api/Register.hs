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
import           Servant.Server              (err404, errReasonPhrase)

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
                                                 (Just uuid) (Just date)
            sendConfirmationToken $ reEmail user
            return uuid
        | otherwise = throw $ PasswordNotMatch $ "password doesn't match with confirmation"

confirmation :: String -> String -> App String
confirmation email token = do
    maybeUser <- runDb (selectFirst [UserEmail ==. email, UserConfirmationToken ==. Just token] [])
    case maybeUser of
        Nothing   -> throwError err404 { errReasonPhrase = "User Not Found in Database" }
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

            bodyText <- bodyTextConfirmationEmail email uuid
            bodyHtml <- bodyHtmlConfirmationEmail email uuid
            liftIO $ sendEmail (ToEmail (fullName $ entityVal user) email)  "Confirmation Email" 
                               bodyText bodyHtml
            return uuid

bodyTextConfirmationEmail :: String -> String -> App String
bodyTextConfirmationEmail email uuid = do
        host <- liftIO getHost
        let fullHost = host ++ "confirmation/" ++ email ++ "/" ++ uuid
        return $ "Thanks for registering, please go to " ++ fullHost ++ " to confirm your subscription."

bodyHtmlConfirmationEmail :: String -> String -> App String
bodyHtmlConfirmationEmail email uuid = do
        host <- liftIO getHost
        let fullHost = host ++ "confirmation/" ++ email ++ "/" ++ uuid
        return $ "Thanks for registering, please go to <a href='" ++ fullHost ++ "'>" ++
                 fullHost ++ "</a> to confirm your subscription."
