{-# LANGUAGE  RecordWildCards #-}

-- |
-- Module:      Pacifica.Robinhood.Migration.Conversions
-- Copyright:   (c) 2017 Pacific Northwest National Laboratory
-- License:     LGPL
-- Maintainer:  Mark Borkum <mark.borkum@pnnl.gov>
-- Stability:   experimental
-- Portability: portable
--
-- This module exports Haskell functions for converting between types for Pacifica/Robinhood migrations.
--
module Pacifica.Robinhood.Migration.Conversions where

import qualified Data.Text
import qualified Data.Text.Encoding
import           Database.Persist.MySQL (MySQLConnectInfo)
import qualified Database.Persist.MySQL
import           Ldap.Client (Ldap, LdapError)
import qualified Ldap.Client
import           Network.Curl.Client (CurlClientEnv(..), CurlCmdSpec(..))
import           Network.URL (Host(..), Protocol(..), URLType(Absolute))
import           Pacifica.Robinhood.Migration.Types (CurlClientConfig(..), LdapClientConfig(..), MySQLConfig(..))
import           System.Process (CmdSpec(RawCommand))

-- | Converts a cURL client configuration into an environment for the cURL client monad transformer.
--
fromCurlClientConfig :: CurlClientConfig -> CurlClientEnv
fromCurlClientConfig CurlClientConfig{..} = CurlClientEnv spec url_type0
  where
    -- | The cURL command specification.
    --
    spec :: CurlCmdSpec
    spec = CurlCmdSpec $ RawCommand
      (Data.Text.unpack _curlClientConfigCommandPath)
      (map Data.Text.unpack _curlClientConfigCommandArguments)
    -- | The URL type.
    --
    url_type0 :: URLType
    url_type0 = Absolute $ Host prot0 (Data.Text.unpack _curlClientConfigHost) _curlClientConfigPort
      where
        -- | If present, convert "ftp[s]" and "http[s]" into data object. Otherwise, return "raw protocol".
        --
        prot0 :: Protocol
        prot0 = case Data.Text.unpack _curlClientConfigProtocol of
          "ftp" -> FTP False
          "ftps" -> FTP True
          "http" -> HTTP False
          "https" -> HTTP True
          x -> RawProt x
{-# INLINE  fromCurlClientConfig #-}

-- | Converts a MySQL configuration into a "connection information" data object.
--
fromMySQLConfig :: MySQLConfig -> MySQLConnectInfo
fromMySQLConfig MySQLConfig{..} = Database.Persist.MySQL.mkMySQLConnectInfo
  (Data.Text.unpack _mySQLConfigHost)
  (Data.Text.Encoding.encodeUtf8 _mySQLConfigUsername)
  (Data.Text.Encoding.encodeUtf8 _mySQLConfigPassword)
  (Data.Text.Encoding.encodeUtf8 _mySQLConfigDatabaseName)
{-# INLINE  fromMySQLConfig #-}

-- | Converts a LDAP configuration into a deferred computation in the 'IO' monad.
--
withLdapClientConfig :: LdapClientConfig -> (Ldap -> IO a) -> IO (Either LdapError a)
withLdapClientConfig LdapClientConfig{..} = Ldap.Client.with (Ldap.Client.Plain $ Data.Text.unpack _ldapClientConfigHost) (fromInteger _ldapClientConfigPort)
{-# INLINE  withLdapClientConfig #-}
