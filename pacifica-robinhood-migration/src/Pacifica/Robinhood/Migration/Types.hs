{-# LANGUAGE  OverloadedStrings #-}
{-# LANGUAGE  RecordWildCards #-}

-- |
-- Module:      Pacifica.Robinhood.Migration.Types
-- Copyright:   (c) 2017 Pacific Northwest National Laboratory
-- License:     LGPL
-- Maintainer:  Mark Borkum <mark.borkum@pnnl.gov>
-- Stability:   experimental
-- Portability: portable
--
-- This module exports Haskell types for Pacifica/Robinhood migrations.
--
module Pacifica.Robinhood.Migration.Types where

import           Data.Aeson (FromJSON(..), ToJSON(..), (.:), (.=))
import qualified Data.Aeson
import           Data.Map (Map)
import           Data.Text (Text)

-- | The configuration for a Pacifica/Robinhood migration.
--
data Config = Config
  { _configAuthConfig :: AuthConfig -- ^ auth
  } deriving (Eq, Ord, Read, Show)

instance FromJSON Config where
  parseJSON = Data.Aeson.withObject "Config" $ \v -> pure Config
    <*> v .: "auth"
  {-# INLINE  parseJSON #-}

instance ToJSON Config where
  toJSON Config{..} = Data.Aeson.object
    [ "auth" .= _configAuthConfig
    ]
  {-# INLINE  toJSON #-}

-- | The authentication configuration for a Pacifica/Robinhood migration, including: cURL, LDAP and MySQL.
--
data AuthConfig = AuthConfig
  { _authConfigCurlClientConfig :: Map Text CurlClientConfig -- ^ curl-client
  , _authConfigLdapClientConfig :: Map Text LdapClientConfig -- ^ ldap-client
  , _authConfigMySQLConfig :: Map Text MySQLConfig -- ^ mysql
  } deriving (Eq, Ord, Read, Show)

instance FromJSON AuthConfig where
  parseJSON = Data.Aeson.withObject "AuthConfig" $ \v -> pure AuthConfig
    <*> v .: "curl-client"
    <*> v .: "ldap-client"
    <*> v .: "mysql"
  {-# INLINE  parseJSON #-}

instance ToJSON AuthConfig where
  toJSON AuthConfig{..} = Data.Aeson.object
    [ "curl-client" .= _authConfigCurlClientConfig
    , "ldap-client" .= _authConfigLdapClientConfig
    , "mysql" .= _authConfigMySQLConfig
    ]
  {-# INLINE  toJSON #-}

-- | The cURL client configuration for a Pacifica/Robinhood migration.
--
data CurlClientConfig = CurlClientConfig
  { _curlClientConfigCommandPath :: Text -- ^ command_path
  , _curlClientConfigCommandArguments :: [Text] -- ^ command_arguments
  , _curlClientConfigProtocol :: Text -- ^ protocol
  , _curlClientConfigHost :: Text -- ^ host
  , _curlClientConfigPort :: Maybe Integer -- ^ port
  } deriving (Eq, Ord, Read, Show)

instance FromJSON CurlClientConfig where
  parseJSON = Data.Aeson.withObject "CurlClientConfig" $ \v -> pure CurlClientConfig
    <*> v .: "command_path"
    <*> v .: "command_arguments"
    <*> v .: "protocol"
    <*> v .: "host"
    <*> v .: "port"
  {-# INLINE  parseJSON #-}

instance ToJSON CurlClientConfig where
  toJSON CurlClientConfig{..} = Data.Aeson.object
    [ "command_path" .= _curlClientConfigCommandPath
    , "command_arguments" .= _curlClientConfigCommandArguments
    , "protocol" .= _curlClientConfigProtocol
    , "host" .= _curlClientConfigHost
    , "port" .= _curlClientConfigPort
    ]
  {-# INLINE  toJSON #-}

-- | The LDAP client configuration for a Pacifica/Robinhood migration.
--
data LdapClientConfig = LdapClientConfig
  { _ldapClientConfigHost :: Text -- ^ host
  , _ldapClientConfigPort :: Integer -- ^ port
  } deriving (Eq, Ord, Read, Show)

instance FromJSON LdapClientConfig where
  parseJSON = Data.Aeson.withObject "LdapClientConfig" $ \v -> pure LdapClientConfig
    <*> v .: "host"
    <*> v .: "port"
  {-# INLINE  parseJSON #-}

instance ToJSON LdapClientConfig where
  toJSON LdapClientConfig{..} = Data.Aeson.object
    [ "host" .= _ldapClientConfigHost
    , "port" .= _ldapClientConfigPort
    ]
  {-# INLINE  toJSON #-}

-- | The MySQL configuration for a Pacifica/Robinhood migration.
--
data MySQLConfig = MySQLConfig
  { _mySQLConfigHost :: Text -- ^ host
  , _mySQLConfigUsername :: Text -- ^ username
  , _mySQLConfigPassword :: Text -- ^ password
  , _mySQLConfigDatabaseName :: Text -- ^ database_name
  } deriving (Eq, Ord, Read, Show)

instance FromJSON MySQLConfig where
  parseJSON = Data.Aeson.withObject "MySQLConfig" $ \v -> pure MySQLConfig
    <*> v .: "host"
    <*> v .: "username"
    <*> v .: "password"
    <*> v .: "database_name"
  {-# INLINE  parseJSON #-}

instance ToJSON MySQLConfig where
  toJSON MySQLConfig{..} = Data.Aeson.object
    [ "host" .= _mySQLConfigHost
    , "username" .= _mySQLConfigUsername
    , "password" .= _mySQLConfigPassword
    , "database_name" .= _mySQLConfigDatabaseName
    ]
  {-# INLINE  toJSON #-}
