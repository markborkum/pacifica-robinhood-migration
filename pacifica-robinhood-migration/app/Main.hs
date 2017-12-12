{-# LANGUAGE  FlexibleContexts #-}
{-# LANGUAGE  GeneralizedNewtypeDeriving #-}
{-# LANGUAGE  OverloadedStrings #-}

-- |
-- Module:      Main
-- Copyright:   (c) 2017 Pacific Northwest National Laboratory
-- License:     LGPL
-- Maintainer:  Mark Borkum <mark.borkum@pnnl.gov>
-- Stability:   experimental
-- Portability: portable
--
-- This module provides the entry-point for the "pacifica-robinhood-migration" executable.
--
-- When built by The Haskell Tool Stack, the executable is invoked using the following command:
--
-- > cat config.json | stack exec pacifica-robinhood-migration-exe
--
-- The configuration for the executable is a JSON document that is provided via
-- the standard input stream. In the above example, the JSON document is persisted
-- as the @config.json@ file.
--
module Main (main) where

import           Control.Monad (join)
import           Control.Monad.Base (MonadBase())
import           Control.Monad.Catch (MonadThrow())
import           Control.Monad.Error.Class (MonadError(throwError))
import           Control.Monad.IO.Class (MonadIO(liftIO))
import           Control.Monad.Logger (LoggingT, runStderrLoggingT)
import           Control.Monad.Reader.Class (MonadReader(ask))
import           Control.Monad.Trans.Except (ExceptT, runExceptT)
import           Control.Monad.Trans.Reader (ReaderT, runReaderT)
import           Control.Monad.Trans.Resource (ResourceT, runResourceT)
import qualified Data.Aeson
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy
import           Data.Conduit (ConduitM, ($$))
import qualified Data.Conduit
import qualified Data.Conduit.List
import qualified Data.Map
import qualified Data.Maybe
import           Data.String (IsString())
import qualified Data.Text.Encoding
import           Data.Void (Void)
import qualified Database.Persist
import           Database.Persist.MySQL (MySQLConnectInfo)
import qualified Database.Persist.MySQL
import           Database.Persist.Sql (SqlBackend)
import qualified Database.Persist.Sql
import           Database.Persist.Types (Entity(..), SelectOpt(LimitTo, OffsetBy))
import           Ldap.Client (Filter(..))
import qualified Ldap.Client
import           Network.Curl.Client (CurlClientEnv, CurlClientT, runCurlClientT, fromCurlRequest)
import           Pacifica.Metadata
import           Pacifica.Metadata.API.Curl
import           Pacifica.Robinhood.Migration
import           Robinhood
import           Robinhood.Extras
import qualified System.Exit
import qualified System.IO
import qualified System.Timeout

-- | Entry-point for the "pacifica-robinhood-migration" executable.
--
main :: IO ()
main = do
  let
    io :: IO ByteString
    io = Data.ByteString.Lazy.readFile "/Users/bork374/Documents/GitHub/pacifica-robinhood-migration/config.json"
  e <- runAppT defAppT io
  case e of
    Left err -> do
      System.IO.hPutStr System.IO.stderr "Error: " >> System.IO.hPrint System.IO.stderr err
      System.Exit.exitFailure
    Right () -> do
      System.Exit.exitSuccess
{-# INLINE  main #-}

newtype AppT m a = AppT (ReaderT AppEnv (ExceptT AppError m) a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader AppEnv, MonadError AppError)

data AppEnv = AppEnv !Config !CurlClientEnv !WrappedLdap !MySQLConnectInfo !MySQLConnectInfo

data AppError
  = CurlClientConfigNotFound !String
  | LdapClientConfigNotFound !String
  | MySQLConfigNotFound !String
  | DecodeConfigFailure !String
  | ReadConfigTimeoutExceeded
  deriving (Eq, Ord, Read, Show)

runAppT :: AppT IO a -> IO ByteString -> IO (Either AppError a)
runAppT (AppT m) = runExceptT . join . fmap (runReaderT m) . fromByteString
  where
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
                Just withLdap -> do
                  -- Convert the MySQL configuration for "archive" database.
                  case fmap fromMySQLConfig $ Data.Map.lookup cMySQLConfigKeyArchive $ _authConfigMySQLConfig $ _configAuthConfig config of
                    -- If the MySQL configuration cannot be converted, then display an error message.
                    Nothing -> do
                      throwError $ MySQLConfigNotFound cMySQLConfigKeyArchive
                    -- Otherwise, continue...
                    Just infoArchive -> do
                      -- Convert the MySQL configuration for "emslfs" database.
                      case fmap fromMySQLConfig $ Data.Map.lookup cMySQLConfigKeyEmslFs $ _authConfigMySQLConfig $ _configAuthConfig config of
                        -- If the MySQL configuration cannot be converted, then display an error message.
                        Nothing -> do
                          throwError $ MySQLConfigNotFound cMySQLConfigKeyEmslFs
                        -- Otherwise, continue...
                        Just infoEmslFs -> do
                          return $ AppEnv config envPacificaMetadata withLdap infoArchive infoEmslFs
    {-# INLINABLE  fromByteString #-}
{-# INLINABLE  runAppT #-}

defAppT :: AppT IO ()
defAppT = AppT $ do
  (AppEnv _config envPacificaMetadata withLdap _infoArchive infoEmslFs) <- ask

  -- Create a new MySQL connection that provides the context for a monadic computation that occurs inside the 'LoggingT' and 'ResourceT' monad transformers.
  --
  -- Notes:
  -- * Using the 'runStderrLoggingT' function, logger output is redirected to the standard error stream.
  runStderrLoggingT $ runResourceT $ Database.Persist.MySQL.withMySQLConn infoEmslFs $ \connEmslFs -> do
    let
      -- | Conduit for streaming rows of the "ENTRIES" database table, viz., instances of the 'Entry' data type.
      --
      -- Notes:
      -- * For purposes of demonstration, stream has fixed limit and offset.
      t :: (MonadBase IO m, MonadIO m, MonadThrow m) => ConduitM () Void (ReaderT SqlBackend (ResourceT (LoggingT m))) ()
      t = Database.Persist.selectSource [] [LimitTo cEntryLimitTo, OffsetBy cEntryOffsetBy] $$ Data.Conduit.List.mapM_ go
        where
          -- | Computation for each instance of the 'Entry' data type.
          --
          -- Notes:
          -- * Computations take place inside an 'IO'-compatible monad.
          -- * Computations return the unit.
          go :: (MonadIO m) => Entity Entry -> m ()
          go (Entity { entityKey = entryId , entityVal = entry }) = do
            -- Print the current 'Entry' to the standard output stream.
            liftIO $ print (entry :: Entry)

            -- Dereference the 'EntryFullPath' for the current 'Entry'.
            entryFullPathMaybe <- liftIO $ Database.Persist.Sql.runSqlPersistM (Database.Persist.Sql.get (EntryFullPathKey entryId)) connEmslFs
            case entryFullPathMaybe of
              Nothing -> do
                liftIO $ putStr "EntryFullPath not found: " >> print entryId
              Just entryFullPath -> do
                liftIO $ print (entryFullPath :: EntryFullPath)

            -- Extract 'uid' attribute of current 'Entry'.
            case entryUid entry of
              Nothing -> do
                return ()
              Just uid -> do
                -- Dereference the 'User' for the current 'Entry' using Pacifica Metadata Services.
                let
                  userM :: (MonadIO m) => CurlClientT (LoggingT m) (Maybe User)
                  userM = fromCurlRequest $ Data.Maybe.listToMaybe <$> readUser Nothing Nothing Nothing Nothing Nothing (Just $ NetworkId uid) Nothing Nothing Nothing (Just 1) (Just 1)
                userEither <- runStderrLoggingT $ runCurlClientT userM envPacificaMetadata
                case userEither of
                  Left err -> do
                    liftIO $ System.IO.hPutStr System.IO.stderr "cURL error: " >> System.IO.hPrint System.IO.stderr err
                  Right Nothing -> do
                    return ()
                  Right (Just user) -> do
                    liftIO $ print (user :: User)

                -- Dereference the 'User' for the current 'Entry' using LDAP.
                ldapEither <- liftIO $ unwrapLdap withLdap $ \connLdap -> do
                  Ldap.Client.search connLdap (Ldap.Client.Dn "ou=People,dc=emsl,dc=pnl,dc=gov") mempty (Ldap.Client.Attr "uid" := Data.Text.Encoding.encodeUtf8 uid)
                    [ Ldap.Client.Attr "memberOf"
                    , Ldap.Client.Attr "objectClass"
                    , Ldap.Client.Attr "mail"
                    , Ldap.Client.Attr "givenName"
                    , Ldap.Client.Attr "sn"
                    , Ldap.Client.Attr "telephoneNumber"
                    , Ldap.Client.Attr "loginShell"
                    , Ldap.Client.Attr "uidNumber"
                    , Ldap.Client.Attr "gidNumber"
                    , Ldap.Client.Attr "uid"
                    , Ldap.Client.Attr "cn"
                    , Ldap.Client.Attr "homeDirectory"
                    ]
                case ldapEither of
                  Left err -> do
                    liftIO $ System.IO.hPutStr System.IO.stderr "LDAP error: " >> System.IO.hPrint System.IO.stderr err
                  Right rsp -> do
                    liftIO $ print rsp

            -- Done!
            return ()

    -- Run the monadic computation.
    runReaderT (Data.Conduit.runConduit t) connEmslFs

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

-- | Limit.
--
cEntryLimitTo :: (Num a) => a
cEntryLimitTo = 10
{-# INLINE  cEntryLimitTo #-}

-- | Offset.
--
cEntryOffsetBy :: (Num a) => a
cEntryOffsetBy = 0
{-# INLINE  cEntryOffsetBy #-}

-- | The timeout for reading the configuration file from the standard input
-- stream (units: milliseconds).
--
cTimeoutMillis :: (Num a) => a
cTimeoutMillis = 10
{-# INLINE  cTimeoutMillis #-}
