{-# LANGUAGE  DeriveDataTypeable #-}
{-# LANGUAGE  RecordWildCards #-}

-- |
-- Module:      Main
-- Copyright:   (c) 2017 Pacific Northwest National Laboratory
-- License:     LGPL
-- Maintainer:  Mark Borkum <mark.borkum@pnnl.gov>
-- Stability:   experimental
-- Portability: portable
--
-- This module provides the entry-point for the "robinhood-treemap-exe" executable.
--
-- When built by The Haskell Tool Stack, the executable is invoked using the following command:
--
-- > stack exec robinhood-treemap-exe -- --depth=3 < filenames.txt > treemap.json
--
-- The input for the executable is a plain-text document that is provided via
-- the standard input stream.  Each line of the plain-text document is a
-- filename.  In the above example, the plain-text document is persisted as the
-- @filenames.txt@ file.
--
-- The output for the executable is a JSON document that is provided via the
-- standard output stream.  In the above example, the JSON document is persisted
-- as the @treemap.json@ file.
--
module Main (main) where

import           Control.Monad.IO.Class (MonadIO(liftIO))
import           Control.Monad.Trans.Resource (runResourceT)
import qualified Data.Aeson (encode)
import qualified Data.ByteString.Lazy (hPut)
import           Data.Conduit (($$), (=$))
import qualified Data.Conduit.Binary (sourceHandle)
import qualified Data.Conduit.List (fold)
import qualified Data.Conduit.Text (decode, foldLines, utf8)
import           Data.Data (Data())
import           Data.Default (Default(def))
import qualified Data.Text (unpack)
import           Data.Typeable (Typeable())
import           Robinhood.Treemap
import           System.Console.CmdArgs.Implicit ((&=))
import qualified System.Console.CmdArgs.Implicit (cmdArgs, explicit, help, name, summary)
import qualified System.Exit (exitSuccess)
import qualified System.IO (hPutStrLn, stdin, stdout)

-- | Entry-point.
--
main :: IO ()
main = do
  -- Read the command-line arguments.
  Command{..} <- System.Console.CmdArgs.Implicit.cmdArgs def
  -- Run @conduit@ pipeline.
  runResourceT
      -- Read from standard input stream.
    $ Data.Conduit.Binary.sourceHandle System.IO.stdin
      -- Decode characters as UTF-8.
   $$ Data.Conduit.Text.decode Data.Conduit.Text.utf8
      -- For each line, accumulate the treemap.
   =$ Data.Conduit.Text.foldLines (Data.Conduit.List.fold (\acc t -> insertWithMaxDepth _commandDepth (Data.Text.unpack t) acc)) empty
      -- Serialize treemap with @aeson@, and write JSON encoding to standard
      -- output stream.
   >>= \acc -> liftIO (Data.ByteString.Lazy.hPut System.IO.stdout (Data.Aeson.encode acc) >> System.IO.hPutStrLn System.IO.stdout "")
  -- Done!
  System.Exit.exitSuccess
{-# INLINE  main #-}

-- | A command.
--
data Command = Command
  { _commandDepth :: Maybe Int
  } deriving (Eq, Ord, Read, Show, Data, Typeable)

instance Default Command where
  def = Command
    { _commandDepth = Nothing
      &= System.Console.CmdArgs.Implicit.explicit
      &= System.Console.CmdArgs.Implicit.name "depth"
      &= System.Console.CmdArgs.Implicit.help "Depth"
    } &= System.Console.CmdArgs.Implicit.summary "robinhood-treemap-exe"
  {-# INLINABLE  def #-}
