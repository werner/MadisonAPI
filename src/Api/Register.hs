{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE DeriveGeneric        #-}

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

data AuthUser = AuthUser { auId    :: Int64
                         , auEmail :: String } deriving (Eq, Show, Read, Generic)

instance ToJSON AuthUser
instance FromJSON AuthUser

type API = "register" :> ReqBody '[JSON] User :> Post '[JSON] AuthUser

server :: ServerT API App
server = register

register :: User -> App AuthUser
register user = do
        user' <- runDb $ insert $ User (userEmail user) (userPassword user) Nothing Nothing
        return $ AuthUser (fromSqlKey user') (userEmail user)

authCheck :: BasicAuthCheck AuthUser
authCheck =
  let check (BasicAuthData username password) =
        if username == (C.pack "servant") && password == (C.pack "server")
        then return $ Authorized $ AuthUser 1 "servant"
        else return Unauthorized
  in BasicAuthCheck check
