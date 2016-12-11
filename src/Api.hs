{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE TypeOperators  #-}
{-# LANGUAGE TypeFamilies   #-}

module Api where

import           Control.Monad.Except
import           Control.Monad.Reader             (ReaderT, runReaderT)
import           Control.Monad.Reader.Class
import           Data.Int                         (Int64)
import           Database.Persist.Postgresql      (Entity (..), fromSqlKey, insert,
                                                   selectFirst, selectList, (==.))
import           Network.Wai                      (Application, Request)
import           Servant
import           Servant.API                      ((:<|>) ((:<|>)), (:>), BasicAuth,
                                                  Get, JSON)
import           Servant.API.BasicAuth            (BasicAuthData (BasicAuthData))
import           Servant.API.Experimental.Auth    (AuthProtect)
import           Servant.Server                   (BasicAuthCheck (BasicAuthCheck),
                                                   BasicAuthResult( Authorized
                                                                  , Unauthorized
                                                                  ),
                                                   Context ((:.), EmptyContext),
                                                   err401, err403, errBody, Server,
                                                   serveWithContext, Handler)
import           Servant.Server.Experimental.Auth (AuthHandler, AuthServerData,
                                                   mkAuthHandler)


import           Config                      (App (..), Config (..), convertApp)
import           Models

import           Api.Types
import           Api.Authentication
import           Api.Session
import           Api.User
import qualified Api.Warehouse               as WarehouseApi
import qualified Api.Register                as Register

type API = Api.Session.API :<|> Register.API :<|> Api.User.API :<|> WarehouseApi.API

type instance AuthServerData (AuthProtect "madison-auth") = Api.User.ShowUser

server :: ServerT Api.API App
server = Api.Session.server :<|> Register.server :<|> Api.User.server :<|> WarehouseApi.server

appToServer :: Config -> Server Api.API
appToServer cfg = enter (convertApp cfg) Api.server

appApi :: Proxy Api.API
appApi = Proxy

authServerContext :: Context (AuthHandler Request Api.User.ShowUser ': '[])
authServerContext = authHandler :. EmptyContext

app :: Config -> Application
app cfg =
    serveWithContext appApi authServerContext (appToServer cfg)
