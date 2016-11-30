{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}

module Api.Register where

import           GHC.Generics
import           Data.Aeson
import           Data.Int                    (Int64)
import qualified Data.ByteString.Char8       as C

import           Control.Monad.Catch
import           Servant
import           Servant.Server              (BasicAuthCheck (BasicAuthCheck),
                                              BasicAuthResult( Authorized
                                                             , Unauthorized
                                                             ),
                                              Context ((:.), EmptyContext),
                                              err401, err403, errBody, Server,
                                              serveWithContext, Handler)
import           Servant.API.BasicAuth       (BasicAuthData (BasicAuthData))
import           Database.Persist.Postgresql (Entity (..), fromSqlKey, insert,
                                              selectFirst, selectList, (==.))

import           Data.ByteString             (ByteString)
import           Config                      (App (..), Config (..))
import           Models
import           Api.User
import           Api.Session

type API = "register" :> ReqBody '[JSON] User :> Post '[JSON] Int64

server :: ServerT Api.Register.API App
server = register

register :: User -> App Int64
register user = do
        user' <- runDb $ insert $ User (userEmail user) (userPassword user) Nothing Nothing
        return $ fromSqlKey user'
