{-# LANGUAGE  FlexibleContexts #-}
{-# LANGUAGE  OverloadedStrings #-}
{-# LANGUAGE  TemplateHaskell #-}

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

import           Control.Monad.Base (MonadBase())
import           Control.Monad.Catch (MonadThrow())
import           Control.Monad.IO.Class (MonadIO(liftIO))
import           Control.Monad.Logger (MonadLogger(), runStderrLoggingT)
import qualified Control.Monad.Logger
import           Control.Monad.Reader.Class (ask)
import           Control.Monad.Trans.Control (MonadBaseControl())
import           Control.Monad.Trans.Reader (ReaderT, runReaderT)
import           Control.Monad.Trans.Resource (ResourceT)
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy
import           Data.Conduit (ConduitM)
import qualified Data.List
import qualified Data.Maybe
import qualified Data.Text
import qualified Data.Text.Encoding
import           Data.Void (Void)
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

-- | Entry-point for the "pacifica-robinhood-migration" executable.
--
main :: IO ()
main = do
  let
    k :: (MonadBase IO m, MonadBaseControl IO m, MonadIO m, MonadLogger m, MonadThrow m) => CurlClientEnv -> WrappedLdap -> Entity EntryFullPath -> ConduitM () Void (ReaderT SqlBackend (ResourceT m)) ()
    k envPacificaMetadata wrappedLdap entryFullPathEntity@(Entity { entityKey = EntryFullPathKey entryKey , entityVal = entryFullPathVal }) = do
      $(Control.Monad.Logger.logInfoSH) entryFullPathEntity

      let
        fp :: FilePath
        fp = Data.Text.unpack $ entryFullPathFullPath entryFullPathVal

      if any (`Data.List.isPrefixOf` fp) ["/dmsarc", "/emslfs"]
        then do
          return ()
        else do
          entryValMaybe <- ask >>= runReaderT (Database.Persist.Sql.get entryKey)
          case entryValMaybe of
            Nothing -> do
              return ()
            Just entryVal -> do
              let
                entryEntity :: Entity Entry
                entryEntity = Entity { entityKey = entryKey , entityVal = entryVal }
              $(Control.Monad.Logger.logInfoSH) entryEntity

              case entryUid entryVal of
                Nothing -> do
                  return ()
                Just uid -> do
                  let
                    userM :: (MonadIO m, MonadLogger m) => CurlClientT m (Maybe User)
                    userM = fromCurlRequest $ Data.Maybe.listToMaybe <$> readUser Nothing Nothing Nothing Nothing Nothing (Just $ NetworkId uid) Nothing Nothing Nothing (Just 1) (Just 1)
                  userValEither <- runCurlClientT userM envPacificaMetadata
                  case userValEither of
                    Left err -> do
                      $(Control.Monad.Logger.logWarnSH) err
                    Right Nothing -> do
                      return ()
                    Right (Just userVal) -> do
                      $(Control.Monad.Logger.logInfoSH) (uid, userVal)

                  ldapEither <- liftIO $ unwrapLdap wrappedLdap $ \connLdap -> do
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
                      $(Control.Monad.Logger.logWarnSH) err
                    Right rsp -> do
                      $(Control.Monad.Logger.logInfoSH) rsp
      return ()
    io :: IO ByteString
    io = Data.ByteString.Lazy.getContents
  e <- runAppTFromByteString (selectAppT [] [LimitTo 10, OffsetBy 0] k runStderrLoggingT) io
  case e of
    Left err -> do
      System.IO.hPutStr System.IO.stderr "Error: " >> System.IO.hPrint System.IO.stderr err
      System.Exit.exitFailure
    Right () -> do
      System.Exit.exitSuccess
{-# INLINE  main #-}
