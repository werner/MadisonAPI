{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}

module Api.Session where

import           GHC.TypeLits                             (KnownSymbol)
import           Control.Monad.Catch                      (MonadThrow)
import           Control.Monad.IO.Class                   (MonadIO)
import           Servant.Server.Experimental.Auth.Cookie  (mkServerKey, mkRandomSource, addSession)
import           Servant                                  (Headers, Header, ReqBody, ServerT,
                                                           Post, JSON, (:>))
import           Web.Cookie                               (def)
import           Crypto.Random                            (drgNew)
import           Data.ByteString                          (ByteString)

import           Lib.Authentication
import           Api.User
import           Config
import           Models

type API = "login" :> ReqBody '[JSON] User :> Post '[JSON] (Headers '[Header "madison-auth" ByteString] Api.User.ShowUser)

server :: ServerT Api.Session.API App
server = login

login :: (KnownSymbol e) => User -> App (Headers '[Header e ByteString] Api.User.ShowUser)
login user = do
        authenticated <- authenticate user
        sk <- mkServerKey 16 Nothing
        rs <- mkRandomSource drgNew 1000
        addSession def rs sk (suId authenticated) authenticated
