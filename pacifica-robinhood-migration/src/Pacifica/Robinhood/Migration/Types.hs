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

import           Control.Comonad.Cofree (Cofree(..))
import           Control.Monad.Logger (LogLevel(..))
import           Data.Aeson (FromJSON(..), ToJSON(..), (.:), (.=))
import           Data.Aeson.Types (Pair)
import qualified Data.Aeson (object, withObject)
import           Data.Map (Map)
import           Data.String (IsString())
import           Data.Text (Text)
import qualified Data.Text (intercalate, splitOn, unpack)

-- | The configuration for a Pacifica/Robinhood migration.
--
data Config = Config
  { _configAuthConfig :: AuthConfig -- ^ auth
  , _configFilePathConfig :: FilePathConfig -- ^ filepath
  } deriving (Eq, Ord, Read, Show)

instance FromJSON Config where
  parseJSON = Data.Aeson.withObject "Config" $ \v -> pure Config
    <*> v .: "auth"
    <*> v .: "filepath"
  {-# INLINE  parseJSON #-}

instance ToJSON Config where
  toJSON Config{..} = Data.Aeson.object
    [ "auth" .= _configAuthConfig
    , "filepath" .= _configFilePathConfig
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

-- | The 'FilePath' configuration for a Pacifica/Robinhood migration.
--
data FilePathConfig = FilePathConfig
  { _filePathConfigFilePathPattern :: FilePathPattern
  } deriving (Eq, Ord, Read, Show)

instance FromJSON FilePathConfig where
  parseJSON v = pure FilePathConfig
    <*> parseJSON v
  {-# INLINE  parseJSON #-}

instance ToJSON FilePathConfig where
  toJSON FilePathConfig{..} = toJSON _filePathConfigFilePathPattern
  {-# INLINE  toJSON #-}

-- | A pattern for 'FilePath's.
--
newtype FilePathPattern = FilePathPattern { getFilePathPattern :: Cofree (Map FilePath) [FilePathRule] }
  deriving (Eq, Ord, Read, Show)

instance FromJSON FilePathPattern where
  parseJSON = Data.Aeson.withObject "FilePathPattern" $ \v -> pure FilePathPattern
    <*> (pure (:<)
      <*> v .: "rules"
      <*> (fmap getFilePathPattern <$> v .: "children"))
  {-# INLINE  parseJSON #-}

instance ToJSON FilePathPattern where
  toJSON (FilePathPattern (rs :< m)) = Data.Aeson.object
    [ "rules" .= rs
    , "children" .= fmap FilePathPattern m
    ]
  {-# INLINE  toJSON #-}

-- | A rule for 'FilePath's.
--
data FilePathRule
  = BreakFilePathRule -- ^ "break"
  | PassFilePathRule -- ^ "pass"
  | PrintFilePathRule Text -- "print"
  | LoggerFilePathRule LogLevel Text -- ^ "logger.x" where "x" is in {"debug", "info", "warn", "error"}
  deriving (Eq, Ord, Read, Show)

instance FromJSON FilePathRule where
  parseJSON = Data.Aeson.withObject "FilePathRule" $ \v -> do
    nameText <- v .: cName
    case Data.Text.splitOn "." nameText of
      ["break"] -> pure BreakFilePathRule
      ["pass"] -> pure PassFilePathRule
      ["print"] -> pure PrintFilePathRule
        <*> v .: "message"
      ["logger", t] -> pure (LoggerFilePathRule (toLogLevel t))
        <*> v .: "message"
      _ -> fail $ "Invalid \"" ++ cName ++ "\": " ++ Data.Text.unpack nameText
  {-# INLINE  parseJSON #-}

instance ToJSON FilePathRule where
  toJSON rule = Data.Aeson.object $ (cName .= toName rule) : toPairs rule
    where
      toName :: FilePathRule -> Text
      toName BreakFilePathRule = "break"
      toName PassFilePathRule = "pass"
      toName (PrintFilePathRule _) = "print"
      toName (LoggerFilePathRule lvl _) = Data.Text.intercalate "." ["logger", fromLogLevel lvl]
      {-# INLINE  toName #-}
      toPairs :: FilePathRule -> [Pair]
      toPairs BreakFilePathRule = []
      toPairs PassFilePathRule = []
      toPairs (PrintFilePathRule msg) = ["message" .= msg]
      toPairs (LoggerFilePathRule _ msg) = ["message" .= msg]
      {-# INLINE  toPairs #-}
  {-# INLINE  toJSON #-}

-- | Convert a 'LogLevel' to a textual identifier.
--
fromLogLevel :: LogLevel -> Text
fromLogLevel LevelDebug = "debug"
fromLogLevel LevelInfo = "info"
fromLogLevel LevelWarn = "warn"
fromLogLevel LevelError = "error"
fromLogLevel (LevelOther x) = x
{-# INLINE  fromLogLevel #-}

-- | Convert a textual identifier to a 'LogLevel'.
--
toLogLevel :: Text -> LogLevel
toLogLevel "debug" = LevelDebug
toLogLevel "info" = LevelInfo
toLogLevel "warn" = LevelWarn
toLogLevel "error" = LevelError
toLogLevel x = LevelOther x
{-# INLINE  toLogLevel #-}

cName :: (IsString a) => a
cName = "__name__"
{-# INLINE cName #-}
