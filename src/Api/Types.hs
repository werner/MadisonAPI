{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}

module Api.Types where

import           Servant                          (AuthProtect)
import           Servant.Server.Experimental.Auth (AuthServerData)

type MadisonAuthProtect = AuthProtect "madison-auth"
type MadisonAuthData    = AuthServerData MadisonAuthProtect
