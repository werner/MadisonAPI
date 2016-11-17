{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators              #-}

module Api where

import           Control.Monad.Except
import           Control.Monad.Reader        (ReaderT, runReaderT)
import           Control.Monad.Reader.Class
import           Data.Int                    (Int64)
import           Database.Persist.Postgresql (Entity (..), fromSqlKey, insert,
                                              selectFirst, selectList, (==.))
import           Network.Wai                 (Application)
import           Servant

import           Config                      (App (..), Config (..))
import           Models

import qualified Api.User                    as UserApi
import qualified Api.Warehouse               as WarehouseApi

type API = UserApi.API :<|> WarehouseApi.API

server :: ServerT API App
server = UserApi.server :<|> WarehouseApi.server

appToServer :: Config -> Server API
appToServer cfg = enter (convertApp cfg) server

convertApp :: Config -> App :~> ExceptT ServantErr IO
convertApp cfg = Nat (flip runReaderT cfg . runApp)

appApi :: Proxy API
appApi = Proxy

app :: Config -> Application
app cfg =
    serve appApi (appToServer cfg)
