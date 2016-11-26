{-# LANGUAGE FlexibleContexts     #-}

module SpecSupport where

import           Data.List                         as L
import           Data.Text                         as T
import           Database.Persist.Types
import           Control.Monad.Reader              (MonadIO, MonadReader, ReaderT, ask, runReaderT)
import           Database.Persist.Sql              (SqlPersistM, SqlBackend, runSqlPersistMPool, rawExecute, 
                                                    rawSql, unSingle, connEscapeName)

import           Config
import           Debug.Trace

wipeDB :: ReaderT SqlBackend IO ()
wipeDB = do
    tables     <- getTables
    sqlBackend <- ask

    let escapedTables = Prelude.map (connEscapeName sqlBackend . DBName) tables
        query = T.append (T.pack "TRUNCATE TABLE ")  (T.intercalate (T.pack ", ") escapedTables)

    rawExecute query  []


getTables :: MonadIO IO => ReaderT SqlBackend IO [Text]
getTables = do
    tables <- rawSql (T.pack "SELECT table_name FROM information_schema.tables WHERE table_schema = 'public'") []

    return $ Prelude.map unSingle tables
