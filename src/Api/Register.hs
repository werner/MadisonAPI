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

authCheck :: User -> App AuthUser
authCheck user = do
    maybeUser <- runDb (selectFirst [UserEmail ==. userEmail user] [])
    case maybeUser of
         Nothing ->
            throwError $ err403 { errReasonPhrase = "Invalid Cookie" }
         Just user ->
            return $ AuthUser (fromSqlKey $ entityKey user) (userEmail $ entityVal user)
