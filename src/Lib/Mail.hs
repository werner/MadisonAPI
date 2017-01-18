{-# LANGUAGE OverloadedStrings #-}

module Lib.Mail where

import           Network.Mail.SMTP 
import           Network.Mail.Mime               (Mail)

import           Data.Text
import qualified Data.Text.Lazy                  as Lazy
import           Config

data ToEmail = ToEmail { toName  :: Text
                       , toEmail :: Text  }

from :: IO Address
from = do
    email <- getEmailAddress
    return $ Address Nothing email
cc  = []
bcc = []

mail :: [Address] -> Text -> Lazy.Text -> Lazy.Text -> IO Mail
mail to subject bodyText bodyHtml = do
        from' <- from
        return $ simpleMail from' to cc bcc subject [plainTextPart bodyText, htmlPart bodyHtml]

sendEmail :: ToEmail -> String -> String -> String -> IO ()
sendEmail to subject bodyText bodyHtml = do
        mail' <- mail [Address (Just $ toName to) (toEmail to)]
                      (pack subject) 
                      (Lazy.pack bodyText) (Lazy.pack bodyHtml)
        renderSendMail mail'  
