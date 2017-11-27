{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}

module Database.Persist.Sql.Orphan.PersistUnique
  ()
  where

import Control.Exception (throwIO)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Trans.Reader (ReaderT)
import Database.Persist
import Database.Persist.Sql.Types
import Database.Persist.Sql.Raw
import Database.Persist.Sql.Orphan.PersistStore (withRawQuery)
import Database.Persist.Sql.Util (dbColumns, parseEntityValues, updatePersistValue, mkUpdateText, commaSeparated)
import Database.Persist.Class.PersistEntity (SomeField(SomeField, CopyUnlessEq))
import Database.Persist.Class.PersistUnique (defaultUpsertMany)
import qualified Data.Text as T
import qualified Data.Conduit.List as CL
import Control.Monad.Trans.Reader (ask, withReaderT)
import Data.Monoid ((<>))
import Data.Either (partitionEithers)

defaultUpsert
    :: (MonadIO m
       ,PersistEntity record
       ,PersistUniqueWrite backend
       ,PersistEntityBackend record ~ BaseBackend backend)
    => record -> [Update record] -> ReaderT backend m (Entity record)
defaultUpsert record updates = do
    uniqueKey <- onlyUnique record
    upsertBy uniqueKey record updates

instance PersistUniqueWrite SqlBackend where
    upsert record updates = do
      conn <- ask
      uniqueKey <- onlyUnique record
      case connUpsertSql conn of
        Just upsertSql -> case updates of
                            [] -> defaultUpsert record updates
                            _:_ -> do
                                let upds = T.intercalate "," $ map (mkUpdateText conn) updates
                                    sql = upsertSql t upds
                                    vals = (map toPersistValue $ toPersistFields record) ++ (map updatePersistValue updates) ++ (unqs uniqueKey)

                                x <- rawSql sql vals
                                return $ head x
        Nothing -> defaultUpsert record updates
        where
          t = entityDef $ Just record
          unqs uniqueKey = concat $ map (persistUniqueToValues) [uniqueKey]

    deleteBy uniq = do
        conn <- ask
        let sql' = sql conn
            vals = persistUniqueToValues uniq
        rawExecute sql' vals
      where
        t = entityDef $ dummyFromUnique uniq
        go = map snd . persistUniqueToFieldNames
        go' conn x = connEscapeName conn x `mappend` "=?"
        sql conn =
            T.concat
                [ "DELETE FROM "
                , connEscapeName conn $ entityDB t
                , " WHERE "
                , T.intercalate " AND " $ map (go' conn) $ go uniq]

    upsertMany_ [] _  _  =  return ()
    upsertMany_ rs fs us = do
        conn <- ask
        case connUpsertMany_Sql conn of
            Nothing
                -> defaultUpsertMany rs fs us
            Just upsertMagic
                -> uncurry rawExecute
                    $ mkBulkInsertQuery upsertMagic (connEscapeName conn) (mkUpdateText conn) rs fs us

-- | This creates the query for 'upsertMany_'. If you
-- provide an empty list of updates to perform, then it will generate
-- a dummy/no-op update using the first field of the record. This avoids
-- duplicate key exceptions.
mkBulkInsertQuery
    :: PersistEntity record
    => ((T.Text -> T.Text), T.Text)
    -> (DBName -> T.Text)
    -> (Update record -> T.Text)
    -> [record]             -- ^ A list of the records you want to insert, or update
    -> [SomeField record]   -- ^ A list of the fields you want to copy over.
    -> [Update record]      -- ^ A list of the updates to apply that aren't dependent on the record being inserted.
    -> (T.Text, [PersistValue])
mkBulkInsertQuery (excludedField, conflictClause) escapeName mkUpdateText' records fieldValues updates =
    (q, recordValues <> updsValues <> copyUnlessValues)
  where
    mfieldDef x = case x of
        SomeField rec -> Right (fieldDbToText (persistFieldDef rec))
        CopyUnlessEq rec val -> Left (fieldDbToText (persistFieldDef rec), toPersistValue val)
    (fieldsToMaybeCopy, updateFieldNames) = partitionEithers $ map mfieldDef fieldValues
    fieldDbToText = escapeName . fieldDB
    entityDef' = entityDef records
    firstField = case entityFieldNames of
        [] -> error "mkBulkInsertQuery: The entity you're trying to insert does not have any fields."
        (field:_) -> field
    entityFieldNames = map fieldDbToText (entityFields entityDef')
    tableName = escapeName . entityDB $ entityDef'
    copyUnlessValues = map snd fieldsToMaybeCopy
    recordValues = concatMap (map toPersistValue . toPersistFields) records
    recordPlaceholders = commaSeparated $ map (parenWrapped . commaSeparated . map (const "?") . toPersistFields) records
    mkCondFieldSet n _ = T.concat
        [ n
        , "=COALESCE("
        ,   "NULLIF("
        ,     excludedField n
        ,     ","
        ,     "?"
        ,   "),"
        ,   n
        , ")"
        ]
    condFieldSets = map (uncurry mkCondFieldSet) fieldsToMaybeCopy
    fieldSets = map (\n -> T.concat [n, "=", excludedField n]) updateFieldNames
    upds = map mkUpdateText' updates
    updsValues = map (\(Update _ val _) -> toPersistValue val) updates
    updateText = case fieldSets <> upds <> condFieldSets of
        [] -> T.concat [firstField, "=", firstField]
        xs -> commaSeparated xs
    q = T.concat
        [ "INSERT INTO "
        , tableName
        , " ("
        , commaSeparated entityFieldNames
        , ") "
        , " VALUES "
        , recordPlaceholders
        , conflictClause
        , updateText
        ]

parenWrapped :: T.Text -> T.Text
parenWrapped t = T.concat ["(", t, ")"]

instance PersistUniqueWrite SqlWriteBackend where
    upsert record updates = withReaderT persistBackend $ upsert record updates
    deleteBy uniq = withReaderT persistBackend $ deleteBy uniq
    upsertMany_ rs fs us = withReaderT persistBackend $ upsertMany_ rs fs us

instance PersistUniqueRead SqlBackend where
    getBy uniq = do
        conn <- ask
        let sql =
                T.concat
                    [ "SELECT "
                    , T.intercalate "," $ dbColumns conn t
                    , " FROM "
                    , connEscapeName conn $ entityDB t
                    , " WHERE "
                    , sqlClause conn]
            uvals = persistUniqueToValues uniq
        withRawQuery sql uvals $
            do row <- CL.head
               case row of
                   Nothing -> return Nothing
                   Just [] -> error "getBy: empty row"
                   Just vals ->
                       case parseEntityValues t vals of
                           Left err ->
                               liftIO $ throwIO $ PersistMarshalError err
                           Right r -> return $ Just r
      where
        sqlClause conn =
            T.intercalate " AND " $ map (go conn) $ toFieldNames' uniq
        go conn x = connEscapeName conn x `mappend` "=?"
        t = entityDef $ dummyFromUnique uniq
        toFieldNames' = map snd . persistUniqueToFieldNames

instance PersistUniqueRead SqlReadBackend where
    getBy uniq = withReaderT persistBackend $ getBy uniq

instance PersistUniqueRead SqlWriteBackend where
    getBy uniq = withReaderT persistBackend $ getBy uniq

dummyFromUnique :: Unique v -> Maybe v
dummyFromUnique _ = Nothing
