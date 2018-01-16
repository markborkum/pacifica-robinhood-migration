{-# LANGUAGE  DeriveDataTypeable #-}
{-# LANGUAGE  FlexibleContexts #-}
{-# LANGUAGE  OverloadedStrings #-}
{-# LANGUAGE  RecordWildCards #-}
{-# LANGUAGE  TypeFamilies #-}

-- |
-- Module:      Main
-- Copyright:   (c) 2017 Pacific Northwest National Laboratory
-- License:     LGPL
-- Maintainer:  Mark Borkum <mark.borkum@pnnl.gov>
-- Stability:   experimental
-- Portability: portable
--
-- This module provides the entry-point for the "pacifica-robinhood-migration-exe" executable.
--
-- When built by The Haskell Tool Stack, the executable is invoked using the following command:
--
-- > stack exec pacifica-robinhood-migration-exe -- \
-- >   --limit=1024 --offset=0 \
-- >   --curl-client="pacifica-metadata" --ldap-client="active-directory" --mysql="rbh" \
-- >   < config.json > out.txt 2> error.txt
--
-- The configuration for the executable is a JSON document that is provided via
-- the standard input stream. In the above example, the JSON document is
-- persisted as the @config.json@ file.
--
-- The output for the executable is a plain-text file that is provided via the
-- the standard output stream.  The logger writes the standard error stream. In
-- the above example, the output and error streams are persisted as the
-- @out.txt@ and @error.txt@ files, respectively.
--
module Main (main) where

import qualified Control.Monad.Logger (runStderrLoggingT)
import           Control.Monad.Trans.Reader (ReaderT)
import           Control.Monad.Trans.Resource (ResourceT)
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy
import           Data.Conduit (ConduitM)
import           Data.Data (Data())
import           Data.Default (Default(def))
import           Data.Functor (void)
import           Data.String (IsString())
import           Data.Text (Text)
import qualified Data.Text
import           Data.Typeable (Typeable())
import           Data.Void (Void)
import           Database.Persist.Sql (SqlBackend)
import           Database.Persist.Types (Entity(..), Filter)
import           Pacifica.Robinhood.Migration
import           Robinhood.Extras
import           System.Console.CmdArgs.Implicit ((&=))
import qualified System.Console.CmdArgs.Implicit (cmdArgs, explicit, help, name, summary)
import qualified System.Exit
import qualified System.IO

-- | Entry-point for the "pacifica-robinhood-migration-exe" executable.
--
main :: IO ()
main = do
  let
    -- | Read the configuration file from standard input stream.
    --
    io :: IO ByteString
    io = Data.ByteString.Lazy.getContents

    -- | Create the (empty) list of filters.
    --
    filterList :: [Filter EntryFullPath]
    filterList = []

    -- | Handler for each 'EntryFullPath' record.
    --
    handleEntryFullPath :: (AppFunConstraint m EntryFullPath) => AppFunEnv EntryFullPath -> ConduitM () Void (ReaderT SqlBackend (ResourceT m)) ()
    handleEntryFullPath env@(AppFunEnv (AppEnv (Config { _configFilePathConfig = FilePathConfig { _filePathConfigFilePathPattern = x } }) _ _ _) (Entity { entityVal = EntryFullPath { entryFullPathFullPath = fp } })) = void $ runFilePathPattern x "" (Data.Text.unpack fp) env

  -- Read the command-line arguments.
  Command{..} <- System.Console.CmdArgs.Implicit.cmdArgs def

  -- Create and run a new application.
  e <- runAppTFromByteString (streamAppT _commandLimitTo _commandOffsetBy filterList handleEntryFullPath Control.Monad.Logger.runStderrLoggingT) _commandCurlClientConfigKey _commandLdapClientConfigKey _commandMySQLConfigKey io
  case e of
    Left err -> do
      System.IO.hPutStr System.IO.stderr "Error: " >> System.IO.hPrint System.IO.stderr err
      System.Exit.exitFailure
    Right () -> do
      System.Exit.exitSuccess
{-# INLINE  main #-}

-- | A command.
--
data Command = Command
  { _commandLimitTo :: Int
  , _commandOffsetBy :: Int
  , _commandCurlClientConfigKey :: Text
  , _commandLdapClientConfigKey :: Text
  , _commandMySQLConfigKey :: Text
  } deriving (Eq, Ord, Read, Show, Data, Typeable)

instance Default Command where
  def = Command
    { _commandLimitTo = cLimitTo
        &= System.Console.CmdArgs.Implicit.explicit
        &= System.Console.CmdArgs.Implicit.name "limit"
        &= System.Console.CmdArgs.Implicit.help "Limit to"
    , _commandOffsetBy = cOffsetBy
        &= System.Console.CmdArgs.Implicit.explicit
        &= System.Console.CmdArgs.Implicit.name "offset"
        &= System.Console.CmdArgs.Implicit.help "Offset by"
    , _commandCurlClientConfigKey = cCurlClientConfigKey
        &= System.Console.CmdArgs.Implicit.explicit
        &= System.Console.CmdArgs.Implicit.name "curl-client"
        &= System.Console.CmdArgs.Implicit.help "cURL client (Pacifica Metadata Services)"
    , _commandLdapClientConfigKey = cLdapClientConfigKey
        &= System.Console.CmdArgs.Implicit.explicit
        &= System.Console.CmdArgs.Implicit.name "ldap-client"
        &= System.Console.CmdArgs.Implicit.help "LDAP client (Active Directory)"
    , _commandMySQLConfigKey = cMySQLConfigKey
        &= System.Console.CmdArgs.Implicit.explicit
        &= System.Console.CmdArgs.Implicit.name "mysql"
        &= System.Console.CmdArgs.Implicit.help "MySQL (Robinhood Policy Engine)"
    } &= System.Console.CmdArgs.Implicit.summary "pacifica-robinhood-migration-exe"
  {-# INLINABLE  def #-}

-- | Default limit.
--
cLimitTo :: Int
cLimitTo = 1024
{-# INLINE cLimitTo #-}

-- | Default offset.
--
cOffsetBy :: Int
cOffsetBy = 0
{-# INLINE cOffsetBy #-}

-- | Default key for cURL client configuration.
--
cCurlClientConfigKey :: (IsString a) => a
cCurlClientConfigKey = "pacifica-metadata"
{-# INLINE  cCurlClientConfigKey #-}

-- | Default key for LDAP client configuration.
--
cLdapClientConfigKey :: (IsString a) => a
cLdapClientConfigKey = "active-directory"
{-# INLINE  cLdapClientConfigKey #-}

-- | Default key for MySQL configuration.
--
cMySQLConfigKey :: (IsString a) => a
cMySQLConfigKey = "rbh"
{-# INLINE  cMySQLConfigKey #-}
