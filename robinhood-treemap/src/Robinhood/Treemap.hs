{-# LANGUAGE  BangPatterns #-}
{-# LANGUAGE  OverloadedStrings #-}
{-# LANGUAGE  RecordWildCards #-}

-- |
-- Module:      Robinhood.Treemap
-- Copyright:   (c) 2017 Pacific Northwest National Laboratory
-- License:     LGPL
-- Maintainer:  Mark Borkum <mark.borkum@pnnl.gov>
-- Stability:   experimental
-- Portability: portable
--
-- This module is the recommended entry point to the @robinhood-treemap@ library.
--
module Robinhood.Treemap
  ( FilePathTreeMap(..)
  , empty
  , insertWithMaxDepth
  ) where

import           Control.Comonad.Cofree (Cofree(..))
import           Data.Aeson (ToJSON(toJSON), Value, (.=))
import qualified Data.Aeson (object)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict (alter, empty, toList)
import qualified System.FilePath.Posix (hasExtension, splitDirectories)

-- | A treemap for filepaths, viz., the cofree comonad, where the base functor
-- is a map from filepath segments to counts.
--
-- TODO Instances
--
newtype FilePathTreeMap = FilePathTreeMap { getFilePathTreeMap :: Cofree (Map FilePath) Int }

-- | c.f., https://bl.ocks.org/mbostock/4063582
--
instance ToJSON FilePathTreeMap where
  toJSON = go "/" . getFilePathTreeMap
    where
      go :: FilePath -> Cofree (Map FilePath) Int -> Value
      go fp (n :< ns) = case map (uncurry go) (Data.Map.Strict.toList ns) of
        [] -> Data.Aeson.object
          [ "name" .= fp
          , "size" .= n
          ]
        vals -> Data.Aeson.object
          [ "name" .= fp
          , "children" .= vals
          ]
  {-# INLINABLE  toJSON #-}

-- | An empty treemap.
--
empty :: FilePathTreeMap
empty = FilePathTreeMap (0 :< Data.Map.Strict.empty)
{-# INLINABLE  empty #-}

-- | Return a new treemap, where the given filepath has been inserted up to the
-- given depth, if 'Just'.
--
insertWithMaxDepth :: Maybe Int -> FilePath -> FilePathTreeMap -> FilePathTreeMap
insertWithMaxDepth d_max fp = FilePathTreeMap . go (drop 1 (maybe id take d_max (System.FilePath.Posix.splitDirectories fp))) . getFilePathTreeMap
  where
    go :: [FilePath] -> Cofree (Map FilePath) Int -> Cofree (Map FilePath) Int
    go xs ((!n) :< (!ns)) =
      let
        !n' = n + 1
        !ns' = case xs of
          [] -> ns
          (x : new_xs)
            | System.FilePath.Posix.hasExtension x -> ns
            | otherwise -> Data.Map.Strict.alter (Just . go new_xs . maybe (getFilePathTreeMap empty) id) x ns
      in
        n' :< ns'
    {-# INLINABLE  go #-}
{-# INLINABLE  insertWithMaxDepth #-}
