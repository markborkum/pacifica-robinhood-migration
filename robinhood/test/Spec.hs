{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

import           Control.Monad (forM_, liftM, replicateM, when)
import           Control.Monad.Logger (NoLoggingT)
import qualified Control.Monad.Logger
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Resource (ResourceT)
import           Data.Char (isAlphaNum)
import           Data.Maybe (fromJust, isJust)
import           Data.Text (Text)
import qualified Data.Text
import           Database.Persist.Sql (SqlBackend, SqlPersistT)
import qualified Database.Persist.Sql
import qualified Database.Persist.Sqlite
import           Database.Persist.Types (Filter)
import           Robinhood
import           Test.QuickCheck
import           Test.QuickCheck.Monadic

arbitraryAlphaNumChar :: Gen Char
arbitraryAlphaNumChar = arbitrary `suchThat` isAlphaNum
{-# INLINE arbitraryAlphaNumChar #-}

arbitraryAlphaNumString :: Int -> Gen String
arbitraryAlphaNumString n0
  | n0 >= 1 = do
      n <- choose (1, n0)
      replicateM n arbitraryAlphaNumChar
  | otherwise = error "Spec.arbitraryAlphaNumString length must be positive and non-zero"
{-# INLINE arbitraryAlphaNumString #-}

arbitraryAlphaNumText :: Int -> Gen Text
arbitraryAlphaNumText = fmap Data.Text.pack . arbitraryAlphaNumString
{-# INLINE arbitraryAlphaNumText #-}

arbitraryMaybeUsing :: Gen a -> Gen (Maybe a)
arbitraryMaybeUsing gen = do
  m <- arbitrary
  case m of
    Nothing -> return Nothing
    Just () -> liftM Just gen
{-# INLINE arbitraryMaybeUsing #-}

instance Arbitrary AccountStatus where
  arbitrary = pure AccountStatus
    <*> arbitraryAlphaNumText 127 -- `ACCT_STAT`.`uid`
    <*> arbitraryAlphaNumText 127 -- `ACCT_STAT`.`gid`
    <*> arbitrary -- `ACCT_STAT`.`type`
    <*> arbitrary -- `ACCT_STAT`.`size`
    <*> arbitrary -- `ACCT_STAT`.`blocks`
    <*> arbitrary -- `ACCT_STAT`.`count`
    <*> arbitrary -- `ACCT_STAT`.`sz0`
    <*> arbitrary -- `ACCT_STAT`.`sz1`
    <*> arbitrary -- `ACCT_STAT`.`sz32`
    <*> arbitrary -- `ACCT_STAT`.`sz1K`
    <*> arbitrary -- `ACCT_STAT`.`sz32K`
    <*> arbitrary -- `ACCT_STAT`.`sz1M`
    <*> arbitrary -- `ACCT_STAT`.`sz32M`
    <*> arbitrary -- `ACCT_STAT`.`sz1G`
    <*> arbitrary -- `ACCT_STAT`.`sz32G`
    <*> arbitrary -- `ACCT_STAT`.`sz1T`
  {-# INLINE arbitrary #-}

instance Arbitrary AnnexInformation where
  arbitrary = pure AnnexInformation
    <*> arbitraryMaybeUsing (arbitraryAlphaNumText 4095) -- `ANNEX_INFO`.`link`
  {-# INLINE arbitrary #-}

instance Arbitrary Entry where
  arbitrary = pure Entry
    <*> arbitraryMaybeUsing (arbitraryAlphaNumText 127) -- `ENTRIES`.`uid`
    <*> arbitraryMaybeUsing (arbitraryAlphaNumText 127) -- `ENTRIES`.`gid`
    <*> arbitrary -- `ENTRIES`.`size`
    <*> arbitrary -- `ENTRIES`.`blocks`
    <*> arbitrary -- `ENTRIES`.`creation_time`
    <*> arbitrary -- `ENTRIES`.`last_access`
    <*> arbitrary -- `ENTRIES`.`last_mod`
    <*> arbitrary -- `ENTRIES`.`last_mdchange`
    <*> arbitrary -- `ENTRIES`.`type`
    <*> arbitrary -- `ENTRIES`.`mode`
    <*> arbitrary -- `ENTRIES`.`nlink`
    <*> arbitrary -- `ENTRIES`.`md_update`
    <*> arbitrary -- `ENTRIES`.`invalid`
    <*> arbitraryMaybeUsing (arbitraryAlphaNumText 1023) -- `ENTRIES`.`fileclass`
    <*> arbitrary -- `ENTRIES`.`class_update`
  {-# INLINE arbitrary #-}

instance Arbitrary EntryType where
  arbitrary = arbitraryBoundedEnum
  {-# INLINE arbitrary #-}

instance Arbitrary Name where
  arbitrary = pure Name
    <*> arbitrary -- `NAMES`.`id`
    <*> arbitrary -- `NAMES`.`parent_id`
    <*> arbitraryMaybeUsing (arbitraryAlphaNumText 255) -- `NAMES`.`name`
    <*> arbitrary -- `NAMES`.`path_update`
  {-# INLINE arbitrary #-}

instance Arbitrary SoftRemove where
  arbitrary = pure SoftRemove
    <*> arbitraryMaybeUsing (arbitraryAlphaNumText 4095) -- `SOFT_RM`.`fullpath`
    <*> arbitraryMaybeUsing (arbitraryAlphaNumText 127) -- `SOFT_RM`.`uid`
    <*> arbitraryMaybeUsing (arbitraryAlphaNumText 127) -- `SOFT_RM`.`gid`
    <*> arbitrary -- `SOFT_RM`.`size`
    <*> arbitrary -- `SOFT_RM`.`blocks`
    <*> arbitrary -- `SOFT_RM`.`last_access`
    <*> arbitrary -- `SOFT_RM`.`last_mod`
    <*> arbitrary -- `SOFT_RM`.`last_mdchange`
    <*> arbitrary -- `SOFT_RM`.`type`
    <*> arbitrary -- `SOFT_RM`.`mode`
    <*> arbitrary -- `SOFT_RM`.`nlink`
    <*> arbitrary -- `SOFT_RM`.`rm_time`
  {-# INLINE arbitrary #-}

instance Arbitrary Variable where
  arbitrary = pure Variable
    <*> arbitraryMaybeUsing (arbitraryAlphaNumText 4095) -- `VARS`.`value`
  {-# INLINE arbitrary #-}

instance Arbitrary (Key AccountStatus) where
  arbitrary = pure AccountStatusKey
    <*> arbitraryAlphaNumText 127 -- `ACCT_STAT`.`uid`
    <*> arbitraryAlphaNumText 127 -- `ACCT_STAT`.`gid`
    <*> arbitrary -- `ACCT_STAT`.`type`
  {-# INLINE arbitrary #-}

instance Arbitrary (Key AnnexInformation) where
  arbitrary = pure AnnexInformationKey
    <*> arbitraryAlphaNumText 64 -- `ANNEX_INFO`.`id`
  {-# INLINE arbitrary #-}

instance Arbitrary (Key Entry) where
  arbitrary = pure EntryKey
    <*> arbitraryAlphaNumText 64 -- `ENTRIES`.`id`
  {-# INLINE arbitrary #-}

instance Arbitrary (Key Name) where
  arbitrary = pure NameKey
    <*> arbitraryAlphaNumText 40 -- `NAMES`.`pkn`
  {-# INLINE arbitrary #-}

instance Arbitrary (Key SoftRemove) where
  arbitrary = pure SoftRemoveKey
    <*> arbitraryAlphaNumText 64 -- `SOFT_RM`.`id`
  {-# INLINE arbitrary #-}

instance Arbitrary (Key Variable) where
  arbitrary = pure VariableKey
    <*> arbitraryAlphaNumText 255 -- `VARS`.`varname`
  {-# INLINE arbitrary #-}

newtype EntityWithKey a = EntityWithKey { runEntityWithKey :: (Key a, a) }

deriving instance (Eq (Key a), Eq a) => Eq (EntityWithKey a)
deriving instance (Ord (Key a), Ord a) => Ord (EntityWithKey a)
deriving instance (Read (Key a), Read a) => Read (EntityWithKey a)
deriving instance (Show (Key a), Show a) => Show (EntityWithKey a)

instance (Arbitrary (Key a), Arbitrary a) => Arbitrary (EntityWithKey a) where
  arbitrary = pure EntityWithKey <*> (pure (,) <*> arbitrary <*> arbitrary)
  {-# INLINE arbitrary #-}

prop_insertKey_ACCT_STAT :: PropertyM (SqlPersistT (NoLoggingT (ResourceT IO))) ()
prop_insertKey_ACCT_STAT = do
  (entityId, entity0) <- runEntityWithKey <$> pick arbitrary

  -- The primary key for records in the `ACCT_STAT` table is a compound of the
  -- following columns: `uid`, `gid` and `type`.  Since both the entity and its
  -- key are arbitrary, it is necessary to replace the columns in the entity
  -- with those in key.
  let
    entity :: AccountStatus
    entity = entity0
      { accountStatusUid = accountStatusKeyuid entityId
      , accountStatusGid = accountStatusKeygid entityId
      , accountStatusType = accountStatusKeytype entityId
      }

  count <- run $ Database.Persist.Sql.count ([] :: [Filter AccountStatus])
  run $ Database.Persist.Sql.insertKey (entityId :: Key AccountStatus) (entity :: AccountStatus)
  count' <- run $ Database.Persist.Sql.count ([] :: [Filter AccountStatus])
  assert $ count' == count + 1

  entity' <- run $ Database.Persist.Sql.get entityId
  assert $ entity' == Just entity

  return ()
{-# INLINE prop_insertKey_ACCT_STAT #-}

prop_insertKey_ANNEX_INFO :: PropertyM (SqlPersistT (NoLoggingT (ResourceT IO))) ()
prop_insertKey_ANNEX_INFO = do
  (entityId, entity) <- runEntityWithKey <$> pick arbitrary

  count <- run $ Database.Persist.Sql.count ([] :: [Filter AnnexInformation])
  run $ Database.Persist.Sql.insertKey (entityId :: Key AnnexInformation) (entity :: AnnexInformation)
  count' <- run $ Database.Persist.Sql.count ([] :: [Filter AnnexInformation])
  assert $ count' == count + 1

  entity' <- run $ Database.Persist.Sql.get entityId
  assert $ entity' == Just entity

  return ()
{-# INLINE prop_insertKey_ANNEX_INFO #-}

prop_insertKey_ENTRIES :: PropertyM (SqlPersistT (NoLoggingT (ResourceT IO))) ()
prop_insertKey_ENTRIES = do
  (entityId, entity) <- runEntityWithKey <$> pick arbitrary

  count <- run $ Database.Persist.Sql.count ([] :: [Filter Entry])
  run $ Database.Persist.Sql.insertKey (entityId :: Key Entry) (entity :: Entry)
  count' <- run $ Database.Persist.Sql.count ([] :: [Filter Entry])
  assert $ count' == count + 1

  entity' <- run $ Database.Persist.Sql.get entityId
  assert $ entity' == Just entity

  return ()
{-# INLINE prop_insertKey_ENTRIES #-}

prop_insertKey_NAMES :: PropertyM (SqlPersistT (NoLoggingT (ResourceT IO))) ()
prop_insertKey_NAMES = do
  (entityId, entity) <- runEntityWithKey <$> pick arbitrary

  -- The `NAMES` table has two foreign keys: `id` and `parent_id`. Both keys
  -- reference the `ENTRIES` table.  Since the corresponding records may not
  -- exist, it is necessary to replace/insert an empty row at each foreign key.
  let
    defEntry :: Entry
    defEntry = Entry Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
  forM_ [nameEntryId entity, nameParentEntryId entity] $ \maybeEntryId -> do
    when (isJust maybeEntryId) $ do
      run $ Database.Persist.Sql.repsert (fromJust maybeEntryId) defEntry

  count <- run $ Database.Persist.Sql.count ([] :: [Filter Name])
  run $ Database.Persist.Sql.insertKey (entityId :: Key Name) (entity :: Name)
  count' <- run $ Database.Persist.Sql.count ([] :: [Filter Name])
  assert $ count' == count + 1

  entity' <- run $ Database.Persist.Sql.get entityId
  assert $ entity' == Just entity

  return ()
{-# INLINE prop_insertKey_NAMES #-}

prop_insertKey_SOFT_RM :: PropertyM (SqlPersistT (NoLoggingT (ResourceT IO))) ()
prop_insertKey_SOFT_RM = do
  (entityId, entity) <- runEntityWithKey <$> pick arbitrary

  count <- run $ Database.Persist.Sql.count ([] :: [Filter SoftRemove])
  run $ Database.Persist.Sql.insertKey (entityId :: Key SoftRemove) (entity :: SoftRemove)
  count' <- run $ Database.Persist.Sql.count ([] :: [Filter SoftRemove])
  assert $ count' == count + 1

  entity' <- run $ Database.Persist.Sql.get entityId
  assert $ entity' == Just entity

  return ()
{-# INLINE prop_insertKey_SOFT_RM #-}

prop_insertKey_VARS :: PropertyM (SqlPersistT (NoLoggingT (ResourceT IO))) ()
prop_insertKey_VARS = do
  (entityId, entity) <- runEntityWithKey <$> pick arbitrary

  count <- run $ Database.Persist.Sql.count ([] :: [Filter Variable])
  run $ Database.Persist.Sql.insertKey (entityId :: Key Variable) (entity :: Variable)
  count' <- run $ Database.Persist.Sql.count ([] :: [Filter Variable])
  assert $ count' == count + 1

  entity' <- run $ Database.Persist.Sql.get entityId
  assert $ entity' == Just entity

  return ()
{-# INLINE prop_insertKey_VARS #-}

main :: IO ()
main = do
  Control.Monad.Logger.runNoLoggingT $ Database.Persist.Sqlite.withSqliteConn ":memory:" $ \conn -> lift $ do
    -- Migrate the database.
    Database.Persist.Sql.runSqlPersistM (Database.Persist.Sql.runMigration migrateAll) (conn :: SqlBackend)
    -- Test each table.
    mapM_ (quickCheck . monadic (ioProperty . flip Database.Persist.Sql.runSqlPersistM (conn :: SqlBackend)))
      [ prop_insertKey_ACCT_STAT
      , prop_insertKey_ANNEX_INFO
      , prop_insertKey_ENTRIES
      , prop_insertKey_NAMES
      , prop_insertKey_SOFT_RM
      , prop_insertKey_VARS
      ]
{-# INLINE main #-}
