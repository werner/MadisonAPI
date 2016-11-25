{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric     #-}

module Api.Register where

import           GHC.Generics
import           Data.Aeson
import           Data.Int                    (Int64)
import qualified Data.ByteString.Char8       as C

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

import           Config                      (App (..), Config (..))
import           Models
import qualified Api.User                    as ApiUser

data AuthUser = AuthUser { auId    :: Int64
                         , auEmail :: String } deriving (Eq, Show, Read, Generic)

type API = "register" :> ReqBody '[JSON] User :> Post '[JSON] ApiUser.ShowUser

server :: ServerT API App
server = register

register :: User -> App ApiUser.ShowUser
register user = do
        user' <- runDb $ insert $ User (userEmail user) (userPassword user) Nothing Nothing
        return $ ApiUser.ShowUser (fromSqlKey user') (userEmail user)
