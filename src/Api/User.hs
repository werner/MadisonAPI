{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE TypeFamilies          #-}

module Api.User where

import           Data.Monoid
import qualified Data.ByteString.Char8                   as C
import           Control.Monad.Except                    (throwError) 
import           Control.Monad.Reader                    (ReaderT, runReaderT)
import           Database.Persist.Postgresql             (entityVal, Entity (..), fromSqlKey, insert,
                                                          selectFirst, selectList, (==.))
import           Network.Wai                             (Application)
import           Servant                                 ((:>), Get, JSON, ServerT, err404)

import           Api.Types
import           Config                                  (App (..), Config (..))
import           Models.Base
import           Models.User

type API = "users" :> MadisonAuthProtect :> Get '[JSON] ShowUser

server :: ServerT API App
server = singleUser

singleUser :: MadisonAuthData -> App ShowUser
singleUser showUser = do
        sessionDB <- runDb (selectFirst [SessionCookie ==. suId showUser] [])
        showUserBySession sessionDB
