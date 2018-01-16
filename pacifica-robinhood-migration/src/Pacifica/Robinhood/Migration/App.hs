{-# LANGUAGE  BangPatterns #-}
{-# LANGUAGE  ConstraintKinds #-}
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
  ( AppT(..)
  , AppEnv(..)
  , AppError(..)
  , runAppTFromByteString
  , AppFunConstraint
  , AppFunEnv(..)
  , streamAppT
  , runFilePathPattern , runFilePathRule
  ) where

import           Control.Comonad.Cofree (Cofree(..))
import           Control.Monad (join)
import           Control.Monad.Base (MonadBase(..), liftBaseDefault)
import           Control.Monad.Catch (MonadThrow())
import           Control.Monad.Error.Class (MonadError(throwError))
import           Control.Monad.IO.Class (MonadIO(liftIO))
import           Control.Monad.Logger (MonadLogger())
import qualified Control.Monad.Logger (logWithoutLoc)
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
import           Data.Text (Text)
import qualified Data.Text
import qualified Data.Text.IO (hPutStrLn)
import           Database.Persist.MySQL (MySQLConnectInfo)
import           Database.Persist (PersistEntity(), PersistEntityBackend())
import qualified Database.Persist.Sql (count)
import           Database.Persist.Types (Entity, Filter, SelectOpt(LimitTo, OffsetBy))
import           Network.Curl.Client (CurlClientEnv)
import           Pacifica.Robinhood.Migration.Types
import qualified System.FilePath.Glob (compile, match)
import           System.FilePath.Posix ((</>))
import qualified System.FilePath.Posix (hasExtension)
import qualified System.IO (stdout)

-- | The environment for an application.
--
data AppEnv = AppEnv !Config !CurlClientEnv !WrappedLdap !MySQLConnectInfo

-- | An error, thrown by the application at run-time.
--
data AppError
  = CurlClientConfigNotFound !Text
  -- ^ The cURL client configuration was not found (c.f., 'cCurlClientConfigKey').
  | LdapClientConfigNotFound !Text
  -- ^ The LDAP client configuration was not found (c.f., 'cLdapClientConfigKey').
  | MySQLConfigNotFound !Text
  -- ^ The MySQL configuration was not found (c.f., 'cMySQLConfigKeyArchive' and 'cMySQLConfigKeyEmslFs').
  | DecodeConfigFailure !String
  -- ^ The configuration file could not be decoded.
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
fromByteString :: Text -> Text -> Text -> IO ByteString -> ExceptT AppError IO AppEnv
fromByteString kCurlClientConfig kLdapClientConfig kMySQLConfig io = do
  configEither <- liftIO $ fmap Data.Aeson.eitherDecode io
  case configEither of
    -- If the contents cannot be decoded, then display an error message.
    Left err -> do
      throwError $ DecodeConfigFailure err
    -- Otherwise, continue...
    Right config -> do
      -- Convert the cURL client configuration.
      case fmap fromCurlClientConfig $ Data.Map.lookup kCurlClientConfig $ _authConfigCurlClientConfig $ _configAuthConfig config of
        -- If the cURL client configuration cannot be converted, then display an error message.
        Nothing -> do
          throwError $ CurlClientConfigNotFound kCurlClientConfig
        -- Otherwise, continue...
        Just envCurlClient -> do
          -- Convert the LDAP client configuration.
          case fmap withLdapClientConfig $ Data.Map.lookup kLdapClientConfig $ _authConfigLdapClientConfig $ _configAuthConfig config of
            -- If the LDAP client configuration cannot be converted, then display an error message.
            Nothing -> do
              throwError $ LdapClientConfigNotFound kLdapClientConfig
            -- Otherwise, continue...
            Just envLdapClient -> do
              -- Convert the MySQL configuration for MySQL database.
              case fmap fromMySQLConfig $ Data.Map.lookup kMySQLConfig $ _authConfigMySQLConfig $ _configAuthConfig config of
                -- If the MySQL configuration cannot be converted, then display an error message.
                Nothing -> do
                  throwError $ MySQLConfigNotFound kMySQLConfig
                -- Otherwise, continue...
                Just envMySQL -> do
                  return $ AppEnv config envCurlClient envLdapClient envMySQL
{-# INLINABLE  fromByteString #-}

-- | Run an 'AppT' by providing a computation that returns a 'ByteString' whose
-- contents provide the configuration.
--
runAppTFromByteString :: AppT IO () -> Text -> Text -> Text -> IO ByteString -> IO (Either AppError ())
runAppTFromByteString t kCurlClientConfig kLdapClientConfig kMySQLConfig = runExceptT . join . fmap (runReaderT (runAppT t)) . fromByteString kCurlClientConfig kLdapClientConfig kMySQLConfig
{-# INLINABLE  runAppTFromByteString #-}

-- | The constraint for an arbitrary function for an application.
--
type AppFunConstraint m val = (MonadBase IO m, MonadBaseControl IO m, MonadIO m, MonadLogger m, MonadThrow m, PersistEntity val, PersistEntityBackend val ~ SqlBackend)

-- | The environment for an arbitrary function for an application.
--
data AppFunEnv val = AppFunEnv !AppEnv !(Entity val)

-- | Skeleton for 'AppT' that streams all @val@s in a given @persistent@ database.
--
-- The stream is implemented using a sliding window, where the size of the window
-- (limit) and the initial index (offset) is provided by the end-user.
--
-- NOTE This instance uses @LIMIT@ and @OFFSET@ syntax from SQL. If the @OFFSET@
-- is large, then SQL queries may be slow.
--
streamAppT
  :: (AppFunConstraint m val)
  => Int -- ^ LIMIT
  -> Int -- ^ OFFSET
  -> [Filter val] -- ^ WHERE
  -> (AppFunEnv val -> ConduitM () Void (ReaderT SqlBackend (ResourceT m)) ()) -- ^ source
  -> (m () -> AppT IO ()) -- ^ extract
  -> AppT IO ()
streamAppT limitTo0 offsetBy0 filterList k f = do
  env@(AppEnv _config _envCurlClient _envLdapClient envMySQL) <- ask
  f $ runResourceT $ Database.Persist.MySQL.withMySQLConn envMySQL $ runReaderT $ do
      -- Compute the total number of records.
      n <- Database.Persist.Sql.count filterList
      let
        -- Inner loop.
        go !limitTo !offsetBy
          -- If the offset is larger than the total number of records, then terminate.
          | offsetBy >= n = return ()
          -- Otherwise, retrieve and process the next set of records, and schedule the next "slide" (for the window).
          | otherwise = do
              Database.Persist.selectSource filterList [LimitTo limitTo, OffsetBy offsetBy] $$ Data.Conduit.List.mapM_ (k . AppFunEnv env)
              go limitTo (offsetBy + limitTo)
      -- Execute the conduit.
      Data.Conduit.runConduit $ go limitTo0 offsetBy0
{-# INLINABLE  streamAppT #-}

-- | Using the GNU Glob library, is the needle a match for the haystack?
--
-- NOTE Match is successful if needle is empty.
--
-- NOTE If the needle is a directory, i.e., does not have a file extension, then
-- the suffix "/**/*.*" is added automatically.
--
isMatchFilePath
  :: ()
  => FilePath -- ^ needle
  -> FilePath -- ^ haystack
  -> Bool
isMatchFilePath needle haystack = (needle == "") || System.FilePath.Glob.match (System.FilePath.Glob.compile new_needle) haystack
  where
    new_needle :: FilePath
    new_needle
      | System.FilePath.Posix.hasExtension needle = needle
      | otherwise = needle </> "**" </> "*.*"
{-# INLINE  isMatchFilePath #-}

-- | Run a pattern for 'FilePath's.
--
-- NOTE Uses a depth-first search.
--
runFilePathPattern :: (AppFunConstraint m val) => FilePathPattern -> FilePath -> FilePath -> AppFunEnv val -> ConduitM () Void (ReaderT SqlBackend (ResourceT m)) Bool
runFilePathPattern (FilePathPattern (rule :< m)) needle haystack env
  -- If the needle is a match for the haystack, then run the 'FilePathRule's for
  -- the current node, and then run the 'FilePathPattern's for the child nodes.
  | isMatchFilePath needle haystack = runFilePathRule rule needle haystack env (fmap or <$> mapM (uncurry (\new_fp pattern -> runFilePathPattern pattern (needle </> new_fp) haystack env)) . Data.Map.toAscList) (fmap FilePathPattern m)
  -- Otherwise, terminate.
  | otherwise = return False
{-# INLINE  runFilePathPattern #-}

-- | Run a rule for 'FilePath's.
--
runFilePathRule :: (AppFunConstraint m val) => FilePathRule -> FilePath -> FilePath -> AppFunEnv val -> (a -> ConduitM () Void (ReaderT SqlBackend (ResourceT m)) Bool) -> a -> ConduitM () Void (ReaderT SqlBackend (ResourceT m)) Bool
runFilePathRule rule needle haystack env k x =
  let
    ruleS :: Text
    ruleS = Data.Text.pack $ show rule
  in case rule of
    BreakFilePathRule -> do
      -- Short-circuit the evaluation.
      return True
    PassFilePathRule -> do
      -- Do nothing.
      k $! x
    PrintFilePathRule msg -> do
      -- Format the message, and then print to the standard output stream.
      liftIO $ Data.Text.IO.hPutStrLn System.IO.stdout $ formatText ruleS needle haystack msg
      k $! x
    LoggerFilePathRule lvl msg -> do
      -- Format the message, and then log at the specified level.
      Control.Monad.Logger.logWithoutLoc "pacifica-robinhood-migration" lvl $ formatText ruleS needle haystack msg
      k $! x
    AllFilePathRule [] -> do
      return True
    AllFilePathRule (new_rule : rules) -> do
      b <- runFilePathRule new_rule needle haystack env k x
      case b of
        False -> return False
        True -> runFilePathRule (AllFilePathRule rules) needle haystack env k x
    AnyFilePathRule [] -> do
      return False
    AnyFilePathRule (new_rule : rules) -> do
      b <- runFilePathRule new_rule needle haystack env k x
      case b of
        False -> runFilePathRule (AnyFilePathRule rules) needle haystack env k x
        True -> return True
{-# INLINE  runFilePathRule #-}

-- | Format text for printing.
--
-- NOTE Replaces "%needle%" and "%haystack" with @needle@ and @haystack@,
-- respectively.
--
formatText :: Text -> FilePath -> FilePath -> Text -> Text
formatText ruleS needle haystack = Data.Text.replace (Data.Text.pack $ "%" ++ cName ++ "%") ruleS . Data.Text.replace "%needle%" (Data.Text.pack needle) . Data.Text.replace "%haystack%" (Data.Text.pack haystack)
{-# INLINE  formatText #-}
