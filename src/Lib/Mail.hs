{-# LANGUAGE OverloadedStrings #-}

module Lib.Mail where

import           Network.Mail.SMTP 
import           Network.Mail.Mime               (Mail)

import qualified Data.Text                       as Text
import qualified Data.Text.Lazy                  as Lazy

data ToEmail = ToEmail { toName  :: String
                       , toEmail :: String  }

from       = Address Nothing "madison@madison.com"
cc         = []
bcc        = []

mail :: [Address] -> Text.Text -> Lazy.Text -> Lazy.Text -> Mail
mail to subject bodyText bodyHtml = simpleMail from to cc bcc subject 
                                     [plainTextPart bodyText, htmlPart bodyHtml]

sendEmail :: ToEmail -> String -> String -> String -> IO ()
sendEmail to subject bodyText bodyHtml = sendMail "madison.com" $ mail [Address (Just $ Text.pack $ toName to) 
                                                                                (Text.pack $ toEmail to)] 
                                                                       (Text.pack subject) 
                                                                       (Lazy.pack bodyText) (Lazy.pack bodyHtml)
