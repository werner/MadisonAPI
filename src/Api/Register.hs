{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric     #-}

module Api.Register where

import           Control.Exception           (throw)
import           GHC.Generics                (Generic)
import           Data.Aeson                  (ToJSON, FromJSON)
import           Data.Int                    (Int64)
import qualified Data.ByteString.Char8       as C

import           Servant                     ((:>), ReqBody, JSON, Post, ServerT)
import           Database.Persist.Postgresql (fromSqlKey, insert)

import           Config                      (App (..), Config (..))
import           Models
import           Api.User
import           Api.Authentication

data RegisterUser = RegisterUser { reEmail                :: String
                                 , rePassword             :: String 
                                 , rePasswordConfirmation :: String} deriving (Show, Read, Generic)

instance ToJSON   RegisterUser
instance FromJSON RegisterUser

type API = "register" :> ReqBody '[JSON] RegisterUser :> Post '[JSON] Int64

server :: ServerT Api.Register.API App
server = register

register :: RegisterUser -> App Int64
register user
        | (rePassword user) == (rePasswordConfirmation user) = do
            cryptPasswd <- encryptPassword $ rePassword user
            user'       <- runDb $ insert $ User (reEmail user) (C.unpack cryptPasswd) Nothing Nothing
            return $ fromSqlKey user'
        | otherwise = throw $ PasswordNotMatch $ "password doesn't match with confirmation"
