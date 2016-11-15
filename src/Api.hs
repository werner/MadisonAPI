{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators              #-}

module Api (app) where

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

import           Api.User

userApp :: Config -> Application
userApp cfg = serve (Proxy :: Proxy UserAPI) (appToServer cfg)

appToServer :: Config -> Server UserAPI
appToServer cfg = enter (convertApp cfg) userServer

convertApp :: Config -> App :~> ExceptT ServantErr IO
convertApp cfg = Nat (flip runReaderT cfg . runApp)

files :: Application
files = serveDirectory "assets"

type AppAPI = UserAPI :<|> Raw

appApi :: Proxy AppAPI
appApi = Proxy

app :: Config -> Application
app cfg =
    serve appApi (appToServer cfg :<|> files)
