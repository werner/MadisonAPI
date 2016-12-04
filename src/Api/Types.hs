{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}

module Api.Types where

import           GHC.Generics
import           Web.HttpApiData
import           Data.Text                        as Text

import           Data.Aeson
import           Servant
import           Servant.Server.Experimental.Auth (AuthHandler, AuthServerData)

type MadisonAuthProtect = AuthProtect "madison-auth"
type MadisonAuthData    = AuthServerData MadisonAuthProtect
