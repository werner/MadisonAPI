{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE DeriveGeneric        #-}

module Api.Types where

import           GHC.Generics
import           Web.HttpApiData
import           Data.Text                        as Text

import           Servant
import           Servant.Server.Experimental.Auth (AuthHandler, AuthServerData)

type MadisonAuthProtect = AuthProtect "madison-auth"
type MadisonAuthData    = AuthServerData MadisonAuthProtect

data SortOrder = SAsc | SDesc deriving (Read, Show, Generic)

instance FromHttpApiData SortOrder where
        parseUrlPiece sortOrder = Right (read $ Text.unpack sortOrder :: SortOrder)

instance ToHttpApiData SortOrder where
        toUrlPiece = showTextData

