{-# LANGUAGE  DeriveDataTypeable #-}
{-# LANGUAGE  FlexibleContexts #-}
{-# LANGUAGE  RecordWildCards #-}
{-# LANGUAGE  TemplateHaskell #-}

-- |
-- Module:      Main
-- Copyright:   (c) 2017 Pacific Northwest National Laboratory
-- License:     LGPL
-- Maintainer:  Mark Borkum <mark.borkum@pnnl.gov>
-- Stability:   experimental
-- Portability: portable
--
-- This module provides the entry-point for the "scan-robinhood-entries-exe" executable.
--
-- When built by The Haskell Tool Stack, the executable is invoked using the following command:
--
-- > stack exec scan-robinhood-entries-exe --limit=1024 --offset=0 < config.json > filenames.txt
--
-- The configuration for the executable is a JSON document that is provided via
-- the standard input stream. In the above example, the JSON document is
-- persisted as the @config.json@ file.
--
-- The output for the executable is a plain-text document that is provided via
-- the standard output stream. In the above example, the plain-text document is
-- persisted as the @filenames.txt@ file.
--
module Main (main) where

import           Control.Monad.Base (MonadBase())
import           Control.Monad.Catch (MonadThrow())
import           Control.Monad.IO.Class (MonadIO(liftIO))
import           Control.Monad.Logger (MonadLogger(), runStderrLoggingT)
import qualified Control.Monad.Logger
import           Control.Monad.Trans.Control (MonadBaseControl())
import           Control.Monad.Trans.Reader (ReaderT)
import           Control.Monad.Trans.Resource (ResourceT)
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy
import           Data.Conduit (ConduitM)
import           Data.Data (Data())
import           Data.Default (Default(def))
import qualified Data.Text
import           Data.Typeable (Typeable())
import           Data.Void (Void)
import           Database.Persist.Sql (SqlBackend)
import           Database.Persist.Types (Entity(..))
import           Network.Curl.Client (CurlClientEnv)
import           Pacifica.Robinhood.Migration
import           Robinhood.Extras
import           System.Console.CmdArgs.Implicit ((&=))
import qualified System.Console.CmdArgs.Implicit (cmdArgs, explicit, help, name, summary)
import qualified System.Exit
import qualified System.IO

-- | Entry-point.
--
main :: IO ()
main = do
  -- Read the command-line arguments.
  Command{..} <- System.Console.CmdArgs.Implicit.cmdArgs def
  let
    -- | Read the configuration file from standard input stream.
    io :: IO ByteString
    io = Data.ByteString.Lazy.getContents
  -- Create and run a new application that is based on the 'streamAppT' skeleton.
  e <- runAppTFromByteString (streamAppT _commandLimitTo _commandOffsetBy [] handleEntryFullPath runStderrLoggingT) io
  case e of
    Left err -> do
      System.IO.hPutStr System.IO.stderr "Error: " >> System.IO.hPrint System.IO.stderr err
      System.Exit.exitFailure
    Right () -> do
      System.Exit.exitSuccess
{-# INLINE  main #-}

-- | Handler for each 'EntryFullPath' record.
--
handleEntryFullPath
  :: (MonadBase IO m, MonadBaseControl IO m, MonadIO m, MonadLogger m, MonadThrow m)
  => CurlClientEnv
  -> WrappedLdap
  -> Entity EntryFullPath
  -> ConduitM () Void (ReaderT SqlBackend (ResourceT m)) ()
handleEntryFullPath _ _ entryFullPathEntity@(Entity { entityVal = entryFullPathVal }) = do
  -- Log the current entity.
  $(Control.Monad.Logger.logInfoSH) entryFullPathEntity
  let
    -- Extract the file path.
    fp :: FilePath
    fp = Data.Text.unpack $ entryFullPathFullPath entryFullPathVal
  -- Print the file path to the standard output stream.
  liftIO $ System.IO.hPutStrLn System.IO.stdout fp
{-# INLINABLE  handleEntryFullPath #-}

-- | A command.
--
data Command = Command
  { _commandLimitTo :: Int
  , _commandOffsetBy :: Int
  } deriving (Eq, Ord, Read, Show, Data, Typeable)

instance Default Command where
  def = Command
    { _commandLimitTo = 1024
        &= System.Console.CmdArgs.Implicit.explicit
        &= System.Console.CmdArgs.Implicit.name "limit"
        &= System.Console.CmdArgs.Implicit.help "LIMIT TO"
    , _commandOffsetBy = 0
        &= System.Console.CmdArgs.Implicit.explicit
        &= System.Console.CmdArgs.Implicit.name "offset"
        &= System.Console.CmdArgs.Implicit.help "OFFSET BY"
    } &= System.Console.CmdArgs.Implicit.summary "scan-robinhood-entries-exe"
  {-# INLINABLE  def #-}
