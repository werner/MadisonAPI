{-# LANGUAGE OverloadedStrings #-}

module Lib.Mail where

import           Network.Mail.SMTP 
import           Network.Mail.Mime               (Mail)

import qualified Data.Text.Internal          as Text
import qualified Data.Text.Internal.Lazy     as Lazy

from       = Address Nothing "madison@madison.com"
cc         = []
bcc        = []

mail :: [Address] -> Text.Text -> Lazy.Text -> Lazy.Text -> Mail
mail to subject bodyText bodyHtml = simpleMail from to cc bcc subject 
                                     [plainTextPart bodyText, htmlPart bodyHtml]

send :: [Address] -> Text.Text -> Lazy.Text -> Lazy.Text -> IO ()
send to subject bodyText bodyHtml = sendMail "madison.com" $ mail to subject bodyText bodyHtml 
