{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module:      Robinhood.Types
-- Copyright:   (c) 2017 Pacific Northwest National Laboratory
-- License:     LGPL
-- Maintainer:  Mark Borkum <mark.borkum@pnnl.gov>
-- Stability:   experimental
-- Portability: portable
--
-- This module exports Haskell types for working with data from the Robinhood
-- Policy Engine that is persisted in a MySQL database.
--
module Robinhood.Types where

import           Data.Text (Text)
import qualified Database.Persist.TH
import           Robinhood.EntryType (EntryType)

Database.Persist.TH.share [Database.Persist.TH.mkPersist Database.Persist.TH.sqlSettings, Database.Persist.TH.mkMigrate "migrateAll"] [Database.Persist.TH.persistUpperCase|
AccountStatus sql=ACCT_STAT
  uid Text sql=uid
  gid Text sql=gid
  type EntryType sql=type
  size Int Maybe sql=size
  blocks Int Maybe sql=blocks
  count Int Maybe sql=count
  sz0 Int Maybe sql=sz0
  sz1 Int Maybe sql=sz1
  sz32 Int Maybe sql=sz32
  sz1K Int Maybe sql=sz1K
  sz32K Int Maybe sql=sz32K
  sz1M Int Maybe sql=sz1M
  sz32M Int Maybe sql=sz32M
  sz1G Int Maybe sql=sz1G
  sz32G Int Maybe sql=sz32G
  sz1T Int Maybe sql=sz1T
  Primary uid gid type
  deriving Eq
  deriving Show

AnnexInformation sql=ANNEX_INFO
  Id Text sql=id
  link Text Maybe sql=link
  deriving Eq
  deriving Show

Entry sql=ENTRIES
  Id Text sql=id
  uid Text Maybe sql=uid
  gid Text Maybe sql=gid
  size Int Maybe sql=size
  blocks Int Maybe sql=blocks
  creationTime Int Maybe sql=creation_time
  lastAccess Int Maybe sql=last_access
  lastMod Int Maybe sql=last_mod
  lastMdChange Int Maybe sql=last_mdchange
  type EntryType Maybe sql=type
  mode Int Maybe sql=mode
  nLink Int Maybe sql=nlink
  mdUpdate Int Maybe sql=md_update
  invalid Bool Maybe sql=invalid
  fileClass Text Maybe sql=fileclass
  classUpdate Int Maybe sql=class_update
  deriving Eq
  deriving Show

Name sql=NAMES
  Id Text sql=pkn
  entryId EntryId Maybe sql=id
  parentEntryId EntryId Maybe sql=parent_id
  name Text Maybe sql=name
  pathUpdate Int Maybe sql=path_update
  deriving Eq
  deriving Show

SoftRemove sql=SOFT_RM
  Id Text sql=id
  fullPath Text Maybe sql=fullpath
  uid Text Maybe sql=uid
  gid Text Maybe sql=gid
  size Int Maybe sql=size
  blocks Int Maybe sql=blocks
  lastAccess Int Maybe sql=last_access
  lastModified Int Maybe sql=last_mod
  lastMdChange Int Maybe sql=last_mdchange
  type EntryType Maybe sql=type
  mode Int Maybe sql=mode
  nLink Int Maybe sql=nlink
  time Int Maybe sql=rm_time
  deriving Eq
  deriving Show

Variable sql=VARS
  Id Text sql=varname
  value Text Maybe sql=value
  deriving Eq
  deriving Show
|]
