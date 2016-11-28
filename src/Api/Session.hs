{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}

module Api.Session where

import           GHC.TypeLits
import           Data.Serialize                   (Serialize)
import           Control.Monad.Catch
import           Servant.Server.Experimental.Auth         (AuthHandler, AuthServerData, mkAuthHandler)
import           Servant.Server.Experimental.Auth.Cookie
import           Servant                                  (Post, Headers, Header, ReqBody, AuthProtect, ServerT,
                                                           Get, Server, Proxy, JSON, (:>))
import           Web.Cookie
import           Crypto.Random                            (drgNew)
import           Servant.API.ResponseHeaders
import           Data.ByteString                          (ByteString)

import           Api.Authentication
import           Api.User
import           Config
import           Models

type API = "login" :> ReqBody '[JSON] User :> Post '[JSON] (Headers '[Header "madison-auth" ByteString] Api.User.ShowUser)

server :: (MonadThrow App) => ServerT Api.Session.API App
server = login

login :: (MonadThrow App, KnownSymbol e) => User -> App (Headers '[Header e ByteString] Api.User.ShowUser)
login user = do
        authenticated <- authenticate user
        sk <- mkServerKey 16 Nothing
        rs <- mkRandomSource drgNew 1000
        addSession def rs sk (suId authenticated) authenticated
