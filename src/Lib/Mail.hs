{-# LANGUAGE OverloadedStrings #-}

module Lib.Mail where

import           Network.Mail.SMTP 
import           Network.Mail.Mime               (Mail)

import qualified Data.Text                       as Text
import qualified Data.Text.Lazy                  as Lazy
import           Config

data ToEmail = ToEmail { toName  :: String
                       , toEmail :: String  }

from :: IO Address
from = do
    email <- getEmailAddress
    return $ Address Nothing $ Text.pack email
cc  = []
bcc = []

mail :: [Address] -> Text.Text -> Lazy.Text -> Lazy.Text -> IO Mail
mail to subject bodyText bodyHtml = do
        from' <- from
        return $ simpleMail from' to cc bcc subject [plainTextPart bodyText, htmlPart bodyHtml]

sendEmail :: ToEmail -> String -> String -> String -> IO ()
sendEmail to subject bodyText bodyHtml = do
        mail' <- mail [Address (Just $ Text.pack $ toName to) (Text.pack $ toEmail to)]
                      (Text.pack subject) 
                      (Lazy.pack bodyText) (Lazy.pack bodyHtml)
        renderSendMail mail'  
