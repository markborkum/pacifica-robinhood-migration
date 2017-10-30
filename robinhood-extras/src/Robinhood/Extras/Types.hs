{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module:      Robinhood.Extras.Types
-- Copyright:   (c) 2017 Pacific Northwest National Laboratory
-- License:     LGPL
-- Maintainer:  Mark Borkum <mark.borkum@pnnl.gov>
-- Stability:   experimental
-- Portability: portable
--
-- This module exports /extra/ Haskell types for working with data from the
-- Robinhood Policy Engine that is persisted in a MySQL database.
--
module Robinhood.Extras.Types where

import           Data.Text (Text)
import           Data.Time.Clock (UTCTime)
import qualified Database.Persist.TH
import           Robinhood.Types

Database.Persist.TH.share [Database.Persist.TH.mkPersist Database.Persist.TH.sqlSettings, Database.Persist.TH.mkMigrate "migrateAll"] [Database.Persist.TH.persistUpperCase|
EntryFullPath sql=file_path_map
  Id (Key Entry) sql=file_id
  pathId (Key EntryBasename) sql=path_id
  fullPath Text sql=full_path
  addedDate UTCTime sql=added_date
  deriving Eq
  deriving Show

EntryBasename sql=file_path_listing
  Id (Key Entry) sql=path_id
  path Text sql=path
  addedDate UTCTime sql=added_date
  deriving Eq
  deriving Show
|]
