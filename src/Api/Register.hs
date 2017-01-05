{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}

module Api.Register where

import           Control.Monad.IO.Class      (liftIO)
import           Data.Maybe                  (fromMaybe)
import           Control.Exception           (throw)
import           Control.Monad.Except        (throwError)
import           Data.Int                    (Int64)
import qualified Data.ByteString.Char8       as C

import           Servant                     ((:<|>)(..), (:>), ReqBody, JSON, Post, Get, ServerT, Capture)
import           Servant.Server              (err404, errReasonPhrase)

import           Database.Persist.Postgresql (Entity (..), fromSqlKey, insert, updateWhere, selectFirst, entityVal,
                                             (==.), (!=.), (=.))

import           Config                      (App (..), Config (..), getHost)
import           Models.Base
import           Lib.Authentication
import           Models.Register

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
            company     <- runDb $ insert $ Company (fromMaybe "" $ reCompanyName user)
            user'       <- runDb $ insert $ User (reEmail user) (C.unpack cryptPasswd)
                                                 (reFirstName user) (reLastName user) 
                                                 (Just company)
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
