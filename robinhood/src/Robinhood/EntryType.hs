{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module:      Robinhood.EntryType
-- Copyright:   (c) 2017 Pacific Northwest National Laboratory
-- License:     LGPL
-- Maintainer:  Mark Borkum <mark.borkum@pnnl.gov>
-- Stability:   experimental
-- Portability: portable
--
-- This module exports an enumeration of entry types supported by Robinhood Policy Engine.
--
module Robinhood.EntryType where

import           Data.Aeson (FromJSON(parseJSON), ToJSON(toJSON))
import           Data.Aeson.Types (Value(String))
import qualified Data.Aeson.Types
import qualified Data.Text
import qualified Database.Persist.TH

-- | Enumeration of entry types supported by the Robinhood Policy Engine.
--
-- \"An @ENUM@ is a string object with a value chosen from a list of permitted
-- values that are enumerated explicitly in the column specification at table
-- creation time.\" <https://dev.mysql.com/doc/refman/5.7/en/enum.html MySQL 5.7 Reference Manual (Section 11.4.4)>
--
-- The MySQL column specification for entry types is
-- @enum(\'symlink\',\'dir\',\'file\',\'chr\',\'blk\',\'fifo\',\'sock\')@ where
-- the default value is @\'file\'@.
--
data EntryType
  = SymbolicLinkEntryType -- ^ "symlink"
  | DirectoryEntryType -- ^ "dir"
  | FileEntryType -- ^ "file"
  | CharacterEntryType -- ^ "chr"
  | BlockEntryType -- ^ "blk"
  | FIFOEntryType -- ^ "fifo"
  | SocketEntryType -- ^ "sock"
  deriving (Bounded, Enum, Eq, Ord)

-- | Corresponds to MySQL column specification.
--
instance Read EntryType where
  readsPrec _depth string = case string of
    "symlink" -> [(SymbolicLinkEntryType, "")]
    "dir"     -> [(DirectoryEntryType,    "")]
    "file"    -> [(FileEntryType,         "")]
    "chr"     -> [(CharacterEntryType,    "")]
    "blk"     -> [(BlockEntryType,        "")]
    "fifo"    -> [(FIFOEntryType,         "")]
    "sock"    -> [(SocketEntryType,       "")]
    _         -> []
  {-# INLINE readsPrec #-}

-- | Corresponds to MySQL column specification.
--
instance Show EntryType where
  show SymbolicLinkEntryType = "symlink"
  show DirectoryEntryType    = "dir"
  show FileEntryType         = "file"
  show CharacterEntryType    = "chr"
  show BlockEntryType        = "blk"
  show FIFOEntryType         = "fifo"
  show SocketEntryType       = "sock"
  {-# INLINE show #-}

-- | Delegates to 'Read' instance.
--
instance FromJSON EntryType where
  parseJSON value = case value of
    String text -> case reads (Data.Text.unpack text) of
      [(entryType, "")] -> pure entryType
      _ -> Data.Aeson.Types.typeMismatch "EntryType" value
    _ -> Data.Aeson.Types.typeMismatch "EntryType" value
  {-# INLINE parseJSON #-}

-- | Delegates to 'Show' instance.
--
instance ToJSON EntryType where
  toJSON = toJSON . show
  {-# INLINE toJSON #-}

Database.Persist.TH.derivePersistField "EntryType"
