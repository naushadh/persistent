{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies, FlexibleContexts, ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}

module Database.Persist.Class.PersistUnique
  (PersistUniqueRead(..)
  ,PersistUniqueWrite(..)
  ,getByValue
  ,insertBy
  ,insertUniqueEntity
  ,replaceUnique
  ,checkUnique
  ,onlyUnique
  ,defaultUpsertMany
  )
  where

import Database.Persist.Types
import Control.Exception (throwIO)
import Control.Monad (liftM)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.List ((\\))
import Control.Monad.Trans.Reader (ReaderT)
import Database.Persist.Class.PersistStore
import Database.Persist.Class.PersistEntity
import Data.Monoid (mappend)
import Data.Text (unpack, Text)
import qualified Data.Maybe as Maybe
import Data.Functor.Constant (Constant(Constant, getConstant))

-- | Queries against 'Unique' keys (other than the id 'Key').
--
-- Please read the general Persistent documentation to learn how to create
-- 'Unique' keys.
--
-- Using this with an Entity without a Unique key leads to undefined
-- behavior.  A few of these functions require a /single/ 'Unique', so using
-- an Entity with multiple 'Unique's is also undefined. In these cases
-- persistent's goal is to throw an exception as soon as possible, but
-- persistent is still transitioning to that.
--
-- SQL backends automatically create uniqueness constraints, but for MongoDB
-- you must manually place a unique index on a field to have a uniqueness
-- constraint.
--
class (PersistCore backend, PersistStoreRead backend) =>
      PersistUniqueRead backend  where
    -- | Get a record by unique key, if available. Returns also the identifier.
    getBy
        :: (MonadIO m, PersistRecordBackend record backend)
        => Unique record -> ReaderT backend m (Maybe (Entity record))

-- | Some functions in this module ('insertUnique', 'insertBy', and
-- 'replaceUnique') first query the unique indexes to check for
-- conflicts. You could instead optimistically attempt to perform the
-- operation (e.g. 'replace' instead of 'replaceUnique'). However,
--
--  * there is some fragility to trying to catch the correct exception and
--  determing the column of failure;
--
--  * an exception will automatically abort the current SQL transaction.
class (PersistUniqueRead backend, PersistStoreWrite backend) =>
      PersistUniqueWrite backend  where
    -- | Delete a specific record by unique key. Does nothing if no record
    -- matches.
    deleteBy
        :: (MonadIO m, PersistRecordBackend record backend)
        => Unique record -> ReaderT backend m ()
    -- | Like 'insert', but returns 'Nothing' when the record
    -- couldn't be inserted because of a uniqueness constraint.
    insertUnique
        :: (MonadIO m, PersistRecordBackend record backend)
        => record -> ReaderT backend m (Maybe (Key record))
    insertUnique datum = do
        conflict <- checkUnique datum
        case conflict of
            Nothing -> Just `liftM` insert datum
            Just _ -> return Nothing
    -- | Update based on a uniqueness constraint or insert:
    --
    -- * insert the new record if it does not exist;
    -- * If the record exists (matched via it's uniqueness constraint), then update the existing record with the parameters which is passed on as list to the function.
    --
    -- Throws an exception if there is more than 1 uniqueness contraint.
    upsert
        :: (MonadIO m, PersistRecordBackend record backend)
        => record          -- ^ new record to insert
        -> [Update record]  -- ^ updates to perform if the record already exists (leaving
                            -- this empty is the equivalent of performing a 'repsert' on a
                            -- unique key)
        -> ReaderT backend m (Entity record) -- ^ the record in the database after the operation
    upsert record updates = do
        uniqueKey <- onlyUnique record
        upsertBy uniqueKey record updates
    -- | Update based on a given uniqueness constraint or insert:
    --
    -- * insert the new record if it does not exist;
    -- * update the existing record that matches the given uniqueness contraint.
    upsertBy
        :: (MonadIO m, PersistRecordBackend record backend)
        => Unique record   -- ^ uniqueness constraint to find by
        -> record          -- ^ new record to insert
        -> [Update record] -- ^ updates to perform if the record already exists (leaving
                           -- this empty is the equivalent of performing a 'repsert' on a
                           -- unique key)
        -> ReaderT backend m (Entity record) -- ^ the record in the database after the operation
    upsertBy uniqueKey record updates = do
        mrecord <- getBy uniqueKey
        maybe (insertEntity record) (`updateGetEntity` updates) mrecord
      where
        updateGetEntity (Entity k _) upds =
            (Entity k) `liftM` (updateGet k upds)

    -- | Do a bulk insert on the given records in the first parameter. In the event
    -- that a key conflicts with a record currently in the database, the second and
    -- third parameters determine what will happen.
    --
    -- The second parameter is a list of fields to copy from the original value.
    -- This allows you to specify which fields to copy from the record you're trying
    -- to insert into the database to the preexisting row.
    --
    -- The third parameter is a list of updates to perform that are independent of
    -- the value that is provided. You can use this to increment a counter value.
    -- These updates only occur if the original record is present in the database.
    --
    -- === __More details on 'SomeField' usage__
    --
    -- The @['SomeField']@ parameter allows you to specify which fields (and
    -- under which conditions) will be copied from the inserted rows. For
    -- a brief example, consider the following data model and existing data set:
    --
    -- @
    -- Item
    --   name        Text
    --   description Text
    --   price       Double Maybe
    --   quantity    Int Maybe
    --
    --   Primary name
    -- @
    --
    -- > items:
    -- > +------+-------------+-------+----------+
    -- > | name | description | price | quantity |
    -- > +------+-------------+-------+----------+
    -- > | foo  | very good   |       |    3     |
    -- > | bar  |             |  3.99 |          |
    -- > +------+-------------+-------+----------+
    --
    -- This record type has a single natural key on @itemName@. Let's suppose
    -- that we download a CSV of new items to store into the database. Here's
    -- our CSV:
    --
    -- > name,description,price,quantity
    -- > foo,,2.50,6
    -- > bar,even better,,5
    -- > yes,wow,,
    --
    -- We parse that into a list of Haskell records:
    --
    -- @
    -- records =
    --   [ Item { itemName = "foo", itemDescription = ""
    --          , itemPrice = Just 2.50, itemQuantity = Just 6
    --          }
    --   , Item "bar" "even better" Nothing (Just 5)
    --   , Item "yes" "wow" Nothing Nothing
    --   ]
    -- @
    --
    -- The new CSV data is partial. It only includes __updates__ from the
    -- upstream vendor. Our CSV library parses the missing description field as
    -- an empty string. We don't want to override the existing description. So
    -- we can use the 'copyUnlessEmpty' function to say: "Don't update when the
    -- value is empty."
    --
    -- Likewise, the new row for @bar@ includes a quantity, but no price. We do
    -- not want to overwrite the existing price in the database with a @NULL@
    -- value. So we can use 'copyUnlessNull' to only copy the existing values
    -- in.
    --
    -- The final code looks like this:
    -- @
    -- 'insertManyOnDuplicateKeyUpdate' records
    --   [ 'copyUnlessEmpty' ItemDescription
    --   , 'copyUnlessNull' ItemPrice
    --   , 'copyUnlessNull' ItemQuantity
    --   ]
    --   []
    -- @
    --
    -- Once we run that code on the datahase, the new data set looks like this:
    --
    -- > items:
    -- > +------+-------------+-------+----------+
    -- > | name | description | price | quantity |
    -- > +------+-------------+-------+----------+
    -- > | foo  | very good   |  2.50 |    6     |
    -- > | bar  | even better |  3.99 |    5     |
    -- > | yes  | wow         |       |          |
    -- > +------+-------------+-------+----------+
    upsertMany_
        ::( MonadIO m
          , PersistRecordBackend record backend
          , Eq record
          )
        => [record]             -- ^ A list of the records you want to insert, or update
        -> [SomeField record]   -- ^ A list of the fields you want to copy over.
        -> [Update record]      -- ^ A list of the updates to apply that aren't dependent on the record being inserted.
        -> ReaderT backend m ()
    upsertMany_ = defaultUpsertMany

-- | The slow but generic 'upsertMany_' implemetation for any 'PersistUniqueRead'.
defaultUpsertMany
    ::( PersistEntityBackend record ~ BaseBackend backend
      , PersistEntity record
      , MonadIO m
      , Eq record
      , PersistStoreWrite backend
      , PersistUniqueRead backend
      )
    => [record]             -- ^ A list of the records you want to insert, or update
    -> [SomeField record]   -- ^ A list of the fields you want to copy over.
    -> [Update record]      -- ^ A list of the updates to apply that aren't dependent on the record being inserted.
    -> ReaderT backend m ()
defaultUpsertMany [] _  _  = return ()
defaultUpsertMany rs fs us = do
    -- lookup record(s) by their unique key
    mEsOld <- mapM getByValue rs

    -- differentiate pre-existing records and new ones
    let merge (Just x) y = Just (x, y)
        merge _        _ = Nothing
    let mEsOldAndRs = zipWith merge mEsOld rs
    let esOldAndRs = Maybe.catMaybes mEsOldAndRs
    let rsOld = fmap snd esOldAndRs
    let esOld = fmap fst esOldAndRs
    let rsNew = rs \\ rsOld

    -- update `old` records
    let toUpdate e (SomeField f) = [Update f (view e (fieldLens f)) Assign]
        toUpdate e (CopyUnlessEq f v)
          = if val == v
            then []
            else [Update f val Assign]
          where
            val = view e (fieldLens f)
    let usFromFs e = concat $ toUpdate e `fmap` fs
    let doUpdate eOld = update k usAll
          where
            k = entityKey eOld
            usAll = us ++ (usFromFs eOld)
    _ <- mapM doUpdate esOld

    -- insert `new` records
    insertMany_ rsNew

-- | Insert a value, checking for conflicts with any unique constraints.  If a
-- duplicate exists in the database, it is returned as 'Left'. Otherwise, the
-- new 'Key is returned as 'Right'.
insertBy
    :: (MonadIO m
       ,PersistUniqueWrite backend
       ,PersistRecordBackend record backend)
    => record -> ReaderT backend m (Either (Entity record) (Key record))
insertBy val = do
    res <- getByValue val
    case res of
        Nothing -> Right `liftM` insert val
        Just z -> return $ Left z

-- | Insert a value, checking for conflicts with any unique constraints. If a
-- duplicate exists in the database, it is left untouched. The key of the
-- existing or new entry is returned
_insertOrGet :: (MonadIO m, PersistUniqueWrite backend, PersistRecordBackend record backend)
            => record -> ReaderT backend m (Key record)
_insertOrGet val = do
    res <- getByValue val
    case res of
        Nothing -> insert val
        Just (Entity key _) -> return key

-- | Like 'insertEntity', but returns 'Nothing' when the record
-- couldn't be inserted because of a uniqueness constraint.
--
-- @since 2.7.1
insertUniqueEntity
    :: (MonadIO m
       ,PersistRecordBackend record backend
       ,PersistUniqueWrite backend)
    => record -> ReaderT backend m (Maybe (Entity record))
insertUniqueEntity datum =
  fmap (\key -> Entity key datum) `liftM` insertUnique datum

-- | Return the single unique key for a record.
onlyUnique
    :: (MonadIO m
       ,PersistUniqueWrite backend
       ,PersistRecordBackend record backend)
    => record -> ReaderT backend m (Unique record)
onlyUnique record =
    case onlyUniqueEither record of
        Right u -> return u
        Left us ->
            requireUniques record us >>=
            liftIO . throwIO . OnlyUniqueException . show . length

onlyUniqueEither
    :: (PersistEntity record)
    => record -> Either [Unique record] (Unique record)
onlyUniqueEither record =
    case persistUniqueKeys record of
        [u] -> Right u
        us -> Left us

-- | A modification of 'getBy', which takes the 'PersistEntity' itself instead
-- of a 'Unique' record. Returns a record matching /one/ of the unique keys. This
-- function makes the most sense on entities with a single 'Unique'
-- constructor.
getByValue
    :: (MonadIO m
       ,PersistUniqueRead backend
       ,PersistRecordBackend record backend)
    => record -> ReaderT backend m (Maybe (Entity record))
getByValue record =
    checkUniques =<< requireUniques record (persistUniqueKeys record)
  where
    checkUniques [] = return Nothing
    checkUniques (x:xs) = do
        y <- getBy x
        case y of
            Nothing -> checkUniques xs
            Just z -> return $ Just z

requireUniques
    :: (MonadIO m, PersistEntity record)
    => record -> [Unique record] -> m [Unique record]
requireUniques record [] = liftIO $ throwIO $ userError errorMsg
  where
    errorMsg = "getByValue: " `Data.Monoid.mappend` unpack (recordName record) `mappend` " does not have any Unique"

requireUniques _ xs = return xs

-- TODO: expose this to users
recordName
    :: (PersistEntity record)
    => record -> Text
recordName = unHaskellName . entityHaskell . entityDef . Just

-- | Attempt to replace the record of the given key with the given new record.
-- First query the unique fields to make sure the replacement maintains
-- uniqueness constraints.
--
-- Return 'Nothing' if the replacement was made.
-- If uniqueness is violated, return a 'Just' with the 'Unique' violation
--
-- @since 1.2.2.0
replaceUnique
    :: (MonadIO m
       ,Eq record
       ,Eq (Unique record)
       ,PersistRecordBackend record backend
       ,PersistUniqueWrite backend)
    => Key record -> record -> ReaderT backend m (Maybe (Unique record))
replaceUnique key datumNew = getJust key >>= replaceOriginal
  where
    uniqueKeysNew = persistUniqueKeys datumNew
    replaceOriginal original = do
        conflict <- checkUniqueKeys changedKeys
        case conflict of
            Nothing -> replace key datumNew >> return Nothing
            (Just conflictingKey) -> return $ Just conflictingKey
      where
        changedKeys = uniqueKeysNew \\ uniqueKeysOriginal
        uniqueKeysOriginal = persistUniqueKeys original

-- | Check whether there are any conflicts for unique keys with this entity and
-- existing entities in the database.
--
-- Returns 'Nothing' if the entity would be unique, and could thus safely be inserted.
-- on a conflict returns the conflicting key
checkUnique
    :: (MonadIO m
       ,PersistRecordBackend record backend
       ,PersistUniqueRead backend)
    => record -> ReaderT backend m (Maybe (Unique record))
checkUnique = checkUniqueKeys . persistUniqueKeys

checkUniqueKeys
    :: (MonadIO m
       ,PersistEntity record
       ,PersistUniqueRead backend
       ,PersistRecordBackend record backend)
    => [Unique record] -> ReaderT backend m (Maybe (Unique record))
checkUniqueKeys [] = return Nothing
checkUniqueKeys (x:xs) = do
    y <- getBy x
    case y of
        Nothing -> checkUniqueKeys xs
        Just _ -> return (Just x)

-- Lens utils

type Getting r s t a b = (a -> Constant r b) -> s -> Constant r t

view :: s -> Getting a s t a b -> a
view s l = getConstant (l Constant s)
