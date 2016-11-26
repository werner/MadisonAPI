{-# LANGUAGE DataKinds             #-}

module Api.Types where

import           Servant
import           Servant.Server.Experimental.Auth (AuthHandler, AuthServerData)

type MadisonAuthProtect = AuthProtect "madison-auth"
type MadisonAuthData    = AuthServerData MadisonAuthProtect

