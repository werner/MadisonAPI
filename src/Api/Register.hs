{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}

module Api.Register where

import           Data.Int                    (Int64)
import qualified Data.ByteString.Char8       as C

import           Servant                     ((:>), ReqBody, JSON, Post, ServerT)
import           Database.Persist.Postgresql (fromSqlKey, insert)

import           Config                      (App (..), Config (..))
import           Models
import           Api.User

type API = "register" :> ReqBody '[JSON] User :> Post '[JSON] Int64

server :: ServerT Api.Register.API App
server = register

register :: User -> App Int64
register user = do
        user' <- runDb $ insert $ User (userEmail user) (userPassword user) Nothing Nothing
        return $ fromSqlKey user'
