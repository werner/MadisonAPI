{-# LANGUAGE DeriveGeneric     #-}

module Models.Register where

import           Data.Monoid                 ((<>))
import           Control.Monad.IO.Class      (liftIO)
import           GHC.Generics                (Generic)
import           Data.Aeson                  (ToJSON, FromJSON)
import           Data.Time.Clock
import           Data.Time.Calendar

import           Control.Monad.Except        (throwError)
import           Servant.Server              (err404, errReasonPhrase)
import           Database.Persist.Postgresql (fromSqlKey, insert, updateWhere, selectFirst, entityVal,
                                             (==.), (!=.), (=.))
import           Lib.Mail
import           Lib.Authentication
import           Models.Base
import           Models.User
import           Config


data RegisterUser = RegisterUser { reEmail                :: String
                                 , reFirstName            :: Maybe String
                                 , reLastName             :: Maybe String
                                 , reCompanyName          :: Maybe String
                                 , rePassword             :: String 
                                 , rePasswordConfirmation :: String} deriving (Show, Read, Generic)

instance ToJSON   RegisterUser
instance FromJSON RegisterUser

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
        let fullHost = host <> "confirmation/" <> email <> "/" <> uuid
        return $ "Thanks for registering, please go to " <> fullHost <> " to confirm your subscription."

bodyHtmlConfirmationEmail :: String -> String -> App String
bodyHtmlConfirmationEmail email uuid = do
        host <- liftIO getHost
        let fullHost = host <> "confirmation/" <> email <> "/" <> uuid
        return $ "Thanks for registering, please go to <a href='" <> fullHost <> "'>" <>
                 fullHost <> "</a> to confirm your subscription."

expirationDate :: IO UTCTime
expirationDate = do
       now <- getCurrentTime
       let twoHours = 2 * 60 * 60
       return $ twoHours `addUTCTime` now
