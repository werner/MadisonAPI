{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Models where

import           Control.Monad.Reader
import           Data.Maybe
import           Safe
import           Data.Aeson                       (FromJSON, ToJSON)
import           Data.Int                         (Int64)
import           Database.Persist.Quasi
import           Database.Persist.Sql             (runMigration, runSqlPool)
import           Database.Persist.TH              (mkMigrate, mkPersist, persistLowerCase, persistFileWith,
                                                   share, sqlSettings)
import           GHC.Generics                     (Generic)


import           Data.Time (UTCTime)
import           Data.Text (Text)
import           Database.Esqueleto               (select, from, where_, val, orderBy, asc, desc,
                                                  desc, limit, Value(..), unValue, EntityField, Key,
                                                  SqlBackend, ToBackendKey, PersistField, PersistEntityBackend,
                                                  PersistEntity, SqlPersistT, toSqlKey, SqlExpr, OrderBy,
                                                  (^.), (?.), (==.))

import           Api.Types
import           Config

share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "src/entities")

doMigrations :: SqlPersistT IO ()
doMigrations = runMigration migrateAll

runDb :: (MonadReader Config m, MonadIO m) => SqlPersistT IO b -> m b
runDb query = do
    pool <- asks getPool
    liftIO $ runSqlPool query pool

nextScopedId
  :: (PersistEntityBackend val ~ SqlBackend, PersistEntity val,
      ToBackendKey SqlBackend record) =>
     Int64
     -> EntityField val (Key record) -> EntityField val Int -> App Int
nextScopedId userId userIdField scopeIdField = do
        lastScopedId <- getLastScopedId userId userIdField scopeIdField
        return $ succ $ unValue (fromMaybe (Value 1) (headMay lastScopedId))

getLastScopedId
  :: (PersistEntityBackend val ~ SqlBackend, PersistEntity val,
      PersistField Int, ToBackendKey SqlBackend record) =>
     Int64
     -> EntityField val (Key record)
     -> EntityField val Int
     -> App [Value Int]
getLastScopedId userId userIdField scopeIdField = 
        runDb $ select $ from $ \warehouses -> do
            where_ $ warehouses ^. userIdField ==. val (toSqlKey userId)
            orderBy [desc (warehouses ^. scopeIdField)]
            limit 1
            return (warehouses ^. scopeIdField)
