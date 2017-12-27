{-# LANGUAGE  BangPatterns #-}
{-# LANGUAGE  FlexibleContexts #-}
{-# LANGUAGE  FlexibleInstances #-}
{-# LANGUAGE  GeneralizedNewtypeDeriving #-}
{-# LANGUAGE  MultiParamTypeClasses #-}
{-# LANGUAGE  OverloadedStrings #-}
{-# LANGUAGE  TypeFamilies #-}
{-# LANGUAGE  UndecidableInstances #-}

-- |
-- Module:      Pacifica.Robinhood.Migration.App
-- Copyright:   (c) 2017 Pacific Northwest National Laboratory
-- License:     LGPL
-- Maintainer:  Mark Borkum <mark.borkum@pnnl.gov>
-- Stability:   experimental
-- Portability: portable
--
-- This module exports Haskell types and functions for building applications
-- that connect to cURL, LDAP, Pacifica Metadata Services and/or Robinhood.
--
module Pacifica.Robinhood.Migration.App
  ( AppT(..) , runAppTFromByteString , selectAppT , streamAppT
  , AppEnv(..)
  , AppError(..)
  ) where

import           Control.Monad (forM_, join)
import           Control.Monad.Base (MonadBase(..), liftBaseDefault)
import           Control.Monad.Catch (MonadThrow())
import           Control.Monad.Error.Class (MonadError(throwError))
import           Control.Monad.IO.Class (MonadIO(liftIO))
import           Control.Monad.Logger (MonadLogger())
import           Control.Monad.Reader.Class (MonadReader(ask))
import           Control.Monad.Trans.Class (MonadTrans(lift))
import           Control.Monad.Trans.Control (MonadBaseControl(..), MonadTransControl(..), ComposeSt, defaultLiftBaseWith, defaultLiftWith2, defaultRestoreM, defaultRestoreT2)
import           Control.Monad.Trans.Except (ExceptT, runExceptT)
import           Control.Monad.Trans.Reader (ReaderT, runReaderT)
import           Control.Monad.Trans.Resource (ResourceT, runResourceT)
import qualified Data.Aeson
import           Data.ByteString.Lazy (ByteString)
import           Data.Conduit (ConduitM, ($$))
import qualified Data.Conduit
import qualified Data.Conduit.List
import qualified Data.Map
import           Data.Void (Void)
import qualified Database.Persist
import qualified Database.Persist.MySQL
import           Database.Persist.Sql (SqlBackend)
import           Data.String (IsString())
import           Database.Persist.MySQL (MySQLConnectInfo)
import           Database.Persist (PersistEntity(), PersistEntityBackend())
import           Database.Persist.Types (Entity, Filter, SelectOpt(LimitTo, OffsetBy))
import           Network.Curl.Client (CurlClientEnv)
import           Pacifica.Robinhood.Migration.Conversions
import           Pacifica.Robinhood.Migration.Types
import qualified System.Timeout

-- | The environment for an application.
--
data AppEnv = AppEnv !Config [(MySQLConnectInfo, (CurlClientEnv, WrappedLdap))]

-- | An error, thrown by the application at run-time.
--
data AppError
  = CurlClientConfigNotFound !String
  -- ^ The cURL client configuration was not found (c.f., 'cCurlClientConfigKey').
  | LdapClientConfigNotFound !String
  -- ^ The LDAP client configuration was not found (c.f., 'cLdapClientConfigKey').
  | MySQLConfigNotFound !String
  -- ^ The MySQL configuration was not found (c.f., 'cMySQLConfigKeyArchive' and 'cMySQLConfigKeyEmslFs').
  | DecodeConfigFailure !String
  -- ^ The configuration file could not be decoded.
  | ReadConfigTimeoutExceeded
  -- ^ The read operation for the configuration file timed out.
  deriving (Eq, Ord, Read, Show)

-- | The application monad transformer.
--
-- The inner computation has an 'AppEnv' as environment and may throw 'AppError's.
--
newtype AppT m a = AppT { runAppT :: ReaderT AppEnv (ExceptT AppError m) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadLogger, MonadThrow, MonadError AppError, MonadReader AppEnv)

instance MonadTrans AppT where
  lift = AppT . lift . lift
  {-# INLINABLE  lift #-}

instance (MonadBase b m) => MonadBase b (AppT m) where
  liftBase = liftBaseDefault
  {-# INLINABLE  liftBase #-}

instance (MonadBaseControl b m) => MonadBaseControl b (AppT m) where
  type StM (AppT m) a = ComposeSt AppT m a
  liftBaseWith = defaultLiftBaseWith
  {-# INLINABLE  liftBaseWith #-}
  restoreM = defaultRestoreM
  {-# INLINABLE  restoreM #-}

instance MonadTransControl AppT where
  type StT AppT a = StT (ExceptT AppError) (StT (ReaderT AppEnv) a)
  liftWith = defaultLiftWith2 AppT runAppT
  {-# INLINABLE  liftWith #-}
  restoreT = defaultRestoreT2 AppT
  {-# INLINABLE  restoreT #-}

-- | Takes a computation that returns a 'ByteString' as input. Reads, parses and
-- processes the contents of the 'ByteString', and then constructs an 'AppEnv'
-- or throws an 'AppError'.
--
-- Note: This function is *not* exposed to end-users.
--
fromByteString :: IO ByteString -> ExceptT AppError IO AppEnv
fromByteString io = do
  configEitherMaybe <- liftIO $ fmap Data.Aeson.eitherDecode <$> System.Timeout.timeout cTimeoutMillis io
  case configEitherMaybe of
    -- If the timeout is exceeded, then display an error message.
    Nothing -> do
      throwError ReadConfigTimeoutExceeded
    -- If the contents cannot be decoded, then display an error message.
    Just (Left err) -> do
      throwError $ DecodeConfigFailure err
    -- Otherwise, continue...
    Just (Right config) -> do
      -- Convert the cURL client configuration.
      case fmap fromCurlClientConfig $ Data.Map.lookup cCurlClientConfigKey $ _authConfigCurlClientConfig $ _configAuthConfig config of
        -- If the cURL client configuration cannot be converted, then display an error message.
        Nothing -> do
          throwError $ CurlClientConfigNotFound cCurlClientConfigKey
        -- Otherwise, continue...
        Just envPacificaMetadata -> do
          -- Convert the LDAP client configuration.
          case fmap withLdapClientConfig $ Data.Map.lookup cLdapClientConfigKey $ _authConfigLdapClientConfig $ _configAuthConfig config of
            -- If the LDAP client configuration cannot be converted, then display an error message.
            Nothing -> do
              throwError $ LdapClientConfigNotFound cLdapClientConfigKey
            -- Otherwise, continue...
            Just wrappedLdap -> do
              -- Convert the MySQL configuration for "archive" database.
              case fmap fromMySQLConfig $ Data.Map.lookup cMySQLConfigKeyArchive $ _authConfigMySQLConfig $ _configAuthConfig config of
                -- If the MySQL configuration cannot be converted, then display an error message.
                Nothing -> do
                  throwError $ MySQLConfigNotFound cMySQLConfigKeyArchive
                -- Otherwise, continue...
                Just _infoArchive -> do -- TODO (infoArchiveFs, (envPacificaMetadata, wrappedLdap))
                  -- Convert the MySQL configuration for "emslfs" database.
                  case fmap fromMySQLConfig $ Data.Map.lookup cMySQLConfigKeyEmslFs $ _authConfigMySQLConfig $ _configAuthConfig config of
                    -- If the MySQL configuration cannot be converted, then display an error message.
                    Nothing -> do
                      throwError $ MySQLConfigNotFound cMySQLConfigKeyEmslFs
                    -- Otherwise, continue...
                    Just infoEmslFs -> do
                      return $ AppEnv config
                        [ (infoEmslFs, (envPacificaMetadata, wrappedLdap))
                        ]
{-# INLINABLE  fromByteString #-}

-- | Run an 'AppT' by providing a computation that returns a 'ByteString' whose
-- contents provide the configuration.
--
runAppTFromByteString :: AppT IO () -> IO ByteString -> IO (Either AppError ())
runAppTFromByteString t = runExceptT . join . fmap (runReaderT (runAppT t)) . fromByteString
{-# INLINABLE  runAppTFromByteString #-}

-- | Skeleton for 'AppT' that selects all @val@s in a given @persistent@ database.
--
-- The selection is implemented using a single query, i.e., *ALL* records are returned.
--
-- NOTE Despite using @conduit@ to ensure that the end-user only operates upon a
-- single record at a given time, all records are loaded into memory. The 'SelectOpt'
-- type should be used to set the total number of records.
--
selectAppT
  :: (MonadBase IO m, MonadBaseControl IO m, MonadIO m, MonadLogger m, MonadThrow m, PersistEntity val, PersistEntityBackend val ~ SqlBackend)
  => [Filter val]
  -> [SelectOpt val]
  -> (CurlClientEnv -> WrappedLdap -> Entity val -> ConduitM () Void (ReaderT SqlBackend (ResourceT m)) ())
  -> (m () -> AppT IO ())
  -> AppT IO ()
selectAppT filterList selectOptList k f = do
  (AppEnv _config appClientInfoList) <- ask
  f $ runResourceT $ forM_ appClientInfoList $ \ ~(info, r) ->
    Database.Persist.MySQL.withMySQLConn info $ runReaderT $ Data.Conduit.runConduit $ Database.Persist.selectSource filterList selectOptList $$ Data.Conduit.List.mapM_ (uncurry k r)
{-# INLINABLE  selectAppT #-}

-- | Skeleton for 'AppT' that streams all @val@s in a given @persistent@ database.
--
-- The stream is implemented using a sliding window, where the size of the window
-- (limit) and the initial index (offset) is provided by the end-user.
--
-- NOTE This instance uses @LIMIT@ and @OFFSET@ syntax from SQL. If the @OFFSET@
-- is large, then SQL queries may be slow.
--
-- TODO Detect end of stream, and then return.
--
streamAppT
  :: (MonadBase IO m, MonadBaseControl IO m, MonadIO m, MonadLogger m, MonadThrow m, PersistEntity val, PersistEntityBackend val ~ SqlBackend)
  => Int -- ^ LIMIT
  -> Int -- ^ OFFSET
  -> [Filter val] -- ^ WHERE
  -> (CurlClientEnv -> WrappedLdap -> Entity val -> ConduitM () Void (ReaderT SqlBackend (ResourceT m)) ()) -- ^ source
  -> (m () -> AppT IO ()) -- ^ extract
  -> AppT IO ()
streamAppT limitTo0 offsetBy0 filterList k f = do
  (AppEnv _config appClientInfoList) <- ask
  f $ runResourceT $ forM_ appClientInfoList $ \ ~(info, r) ->
    let
      go !limitTo !offsetBy = do
        Database.Persist.MySQL.withMySQLConn info $ runReaderT $ Data.Conduit.runConduit $ Database.Persist.selectSource filterList [LimitTo limitTo, OffsetBy offsetBy] $$ Data.Conduit.List.mapM_ (uncurry k r)
        go limitTo (offsetBy + limitTo)
    in
      go limitTo0 offsetBy0
{-# INLINABLE  streamAppT #-}

-- | Key for cURL client configuration.
--
cCurlClientConfigKey :: (IsString a) => a
cCurlClientConfigKey = "pacifica-metadata"
{-# INLINE  cCurlClientConfigKey #-}

-- | Key for LDAP client configuration.
--
cLdapClientConfigKey :: (IsString a) => a
cLdapClientConfigKey = "active-directory"
{-# INLINE  cLdapClientConfigKey #-}

-- | Key for MySQL configuration for "archive" database.
--
cMySQLConfigKeyArchive :: (IsString a) => a
cMySQLConfigKeyArchive = "rbh_archive"
{-# INLINE  cMySQLConfigKeyArchive #-}

-- | Key for MySQL configuration for "emslfs" database.
--
cMySQLConfigKeyEmslFs :: (IsString a) => a
cMySQLConfigKeyEmslFs = "rbh_emslfs"
{-# INLINE  cMySQLConfigKeyEmslFs #-}

-- | The timeout for reading the configuration file from the standard input
-- stream (units: milliseconds).
--
cTimeoutMillis :: (Num a) => a
cTimeoutMillis = 1000
{-# INLINE  cTimeoutMillis #-}
