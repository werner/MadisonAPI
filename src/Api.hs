{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TypeOperators              #-}

module Api where

import           Control.Monad.Except
import           Control.Monad.Reader             (ReaderT, runReaderT)
import           Control.Monad.Reader.Class
import           Data.Int                         (Int64)
import           Database.Persist.Postgresql      (Entity (..), fromSqlKey, insert,
                                                   selectFirst, selectList, (==.))
import           Network.Wai                      (Application)
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
import           Servant.Server.Experimental.Auth()


import           Config                      (App (..), Config (..))
import           Models

import qualified Api.User                    as UserApi
import qualified Api.Warehouse               as WarehouseApi
import qualified Api.Register                as Register

type API = Register.API 
       :<|> BasicAuth "auth-realm" Register.AuthUser :> UserApi.API
       :<|> BasicAuth "auth-realm" Register.AuthUser :> WarehouseApi.API

server :: ServerT API App
server = Register.server :<|> UserApi.server :<|> WarehouseApi.server

appToServer :: Config -> Server API
appToServer cfg = enter (convertApp cfg) server

convertApp :: Config -> App :~> ExceptT ServantErr IO
convertApp cfg = Nat (flip runReaderT cfg . runApp)

appApi :: Proxy API
appApi = Proxy

authServerContext :: Context (BasicAuthCheck Register.AuthUser ': '[])
authServerContext = Register.authCheck :. EmptyContext

app :: Config -> Application
app cfg =
    serveWithContext appApi authServerContext (appToServer cfg)
