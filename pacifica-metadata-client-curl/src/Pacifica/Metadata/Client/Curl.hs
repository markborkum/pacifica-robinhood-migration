{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module:      Pacifica.Metadata.Client.Curl
-- Copyright:   (c) 2017 Pacific Northwest National Laboratory
-- License:     LGPL
-- Maintainer:  Mark Borkum <mark.borkum@pnnl.gov>
-- Stability:   experimental
-- Portability: portable
--
-- This module exports Haskell types and functions for working with Pacifica Metadata Services using a cURL-based client.
--
-- For example, to request the data from a Pacifica Metadata Services deployment at http://0.0.0.0:8121 for the 'User' with @('UserId' 42)@:
--
-- > import Control.Monad.Trans.Except
-- > import Control.Monad.Trans.Reader
-- > import Data.Default
-- > import Network.URL
-- > import Pacifica.Metadata
-- > import Pacifica.Metadata.Client.Curl
-- >
-- > main :: IO ()
-- > main = do
-- >   let
-- >     spec :: CurlCommandSpec
-- >     spec = def
-- >     env :: CurlClientEnv
-- >     env = CurlClientEnv spec $ Absolute $ Host (HTTP False) "0.0.0.0" (Just 8121) -- http://0.0.0.0:8121
-- >     mx :: CurlClientM (Maybe User)
-- >     mx = getUser $ UserId 42
-- >   x <- runExceptT $ flip runReaderT env $ runCurlClientM mx
-- >   case x of
-- >     Left err -> putStr "Error: " >> print err
-- >     Right Nothing -> putStrLn "Warning: User not found"
-- >     Right (Just user) -> print user
--
module Pacifica.Metadata.Client.Curl
  ( -- * cURL client types
    CurlClientM(..)
  , CurlClientEnv(..)
  , CurlClientException(..)
  , CurlCommandSpec(..)
  , CurlHTTPRequestMethod(..)
  , CurlInvocationContents(..)
    -- * APIs
    -- ** Institutions API
  , getInstitution , getInstitutions
    -- ** Instrument custodians API
  , getInstrumentCustodian , getInstrumentCustodians
    -- ** Journals API
  , getJournal , getJournals
    -- ** Proposals API
  , getProposal , getProposals
    -- ** Users API
  , getUser , getUsers
  ) where

import           Control.Monad.Error.Class (MonadError(throwError))
import           Control.Monad.IO.Class (MonadIO(liftIO))
import           Control.Monad.Reader.Class (MonadReader())
import qualified Control.Monad.Reader.Class
import           Control.Monad.Trans.Except (ExceptT(..))
import           Control.Monad.Trans.Reader (ReaderT(..))
import           Data.Aeson (FromJSON(), ToJSON())
import qualified Data.Aeson
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy
import qualified Data.ByteString.Lazy.Char8
import           Data.Default (Default(def))
import qualified Data.Maybe
import           Data.Proxy (Proxy(..))
import           Data.Text (Text)
import           Data.Time.Calendar (Day)
import           Data.Time.LocalTime (LocalTime)
import           Network.URL (URL(..), URLType)
import qualified Network.URL
import           Pacifica.Metadata.Types
import           System.Exit (ExitCode(..))
import           System.Process (CreateProcess, StdStream(CreatePipe, NoStream))
import qualified System.Process
import qualified Text.Printf

-- | The cURL client monad.
--
newtype CurlClientM a = CurlClientM { runCurlClientM :: ReaderT CurlClientEnv (ExceptT CurlClientException IO) a }
  deriving (Functor, Applicative, Monad, MonadError CurlClientException, MonadIO, MonadReader CurlClientEnv)

-- | Construct a new cURL client whose monadic computation yields the result of parsing JSON data.
--
newCurlClientM :: (FromJSON a) => Proxy a -> CurlHTTPRequestMethod -> String -> [(String, String)] -> CurlClientM a
newCurlClientM Proxy http_request_method0 url_path0 url_params0 = CurlClientM $ do
  cp <- Control.Monad.Reader.Class.asks $ createProcessForCurlClientEnv http_request_method0 url_path0 url_params0
  (Nothing, Just h_std_out, Just h_std_err, h_proc) <- liftIO $ System.Process.createProcess cp
    { System.Process.std_in  = NoStream
    , System.Process.std_out = CreatePipe
    , System.Process.std_err = CreatePipe
    }
  exitCode <- liftIO $ System.Process.waitForProcess h_proc
  std_out <- liftIO $ Data.ByteString.Lazy.hGetContents h_std_out
  std_err <- liftIO $ Data.ByteString.Lazy.hGetContents h_std_err
  let
    raw :: CurlInvocationContents
    raw = CurlInvocationContents std_out std_err
  case exitCode of
    ExitSuccess -> do
      case Data.Aeson.eitherDecode' std_out of
        Left message -> do
          throwError $ CurlClientFromJSONError raw message
        Right result -> do
          return result
    ExitFailure exitStatus -> do
      throwError $ CurlClientNonZeroExitStatus raw exitStatus
{-# INLINE  newCurlClientM #-}

-- | The environment for the cURL client monad.
--
data CurlClientEnv = CurlClientEnv CurlCommandSpec URLType

-- | Create process using environment for cURL client monad.
--
createProcessForCurlClientEnv :: CurlHTTPRequestMethod -> String -> [(String, String)] -> CurlClientEnv -> CreateProcess
createProcessForCurlClientEnv http_request_method0 url_path0 url_params0 (CurlClientEnv spec url_type0) = createProcessForCurlCommandSpec http_request_method0 url spec
  where
    url :: URL
    url = URL url_type0 url_path0 url_params0
{-# INLINE  createProcessForCurlClientEnv #-}

-- | The specification for a cURL command, either: a "shell" command, or a "raw"
-- command consisting of the 'FilePath' to an executable and a list of
-- command-line arguments.
--
data CurlCommandSpec
  = CurlShellCommand String
  | CurlRawCommand FilePath [String]
  deriving (Eq, Ord, Read, Show)

-- | "curl" (no command-line arguments).
--
instance Default CurlCommandSpec where
  def = CurlRawCommand "curl" []
  {-# INLINE  def #-}

-- | Create process using 'URL' and specification for a cURL command.
--
createProcessForCurlCommandSpec :: CurlHTTPRequestMethod -> URL -> CurlCommandSpec -> CreateProcess
createProcessForCurlCommandSpec http_request_method url spec = case spec of
  CurlShellCommand cmd_args -> System.Process.shell $ Text.Printf.printf "%s -X %s %s" cmd_args (show http_request_method) urlString
  CurlRawCommand cmd args -> System.Process.proc cmd $ "-X" : show http_request_method : snoc urlString args
  where
    urlString :: String
    urlString = Network.URL.exportURL url
{-# INLINE  createProcessForCurlCommandSpec #-}

-- | HTTP request methods supported by cURL.
--
data CurlHTTPRequestMethod = GET | HEAD | POST | PUT | DELETE | CONNECT | OPTIONS | TRACE | PATCH
  deriving (Eq, Ord, Read, Show)

-- | An exception raised by the cURL client monad.
--
data CurlClientException
  = CurlClientFromJSONError CurlInvocationContents String
  | CurlClientNonZeroExitStatus CurlInvocationContents Int
  deriving (Eq, Ord, Read, Show)

-- | The contents of a cURL invocation: the standard output and standard error streams.
--
data CurlInvocationContents = CurlInvocationContents
  { _curlInvocationContentsStdOut :: ByteString
  , _curlInvocationContentsStdErr :: ByteString
  } deriving (Eq, Ord, Read, Show)

-- | GET /institutions?_id={_id}
--
getInstitution
  :: ()
  => InstitutionId -- ^ _id
  -> CurlClientM (Maybe Institution)
getInstitution x = fmap safeHead $ getInstitutions (Just x) Nothing Nothing Nothing Nothing Nothing Nothing Nothing
{-# INLINE  getInstitution #-}

-- | GET /institutions
--
getInstitutions
  :: ()
  => Maybe InstitutionId -- ^ _id
  -> Maybe Text -- ^ association_cd
  -> Maybe Text -- ^ encoding
  -> Maybe Bool -- ^ is_foreign
  -> Maybe Text -- ^ name
  -> Maybe LocalTime -- ^ created
  -> Maybe LocalTime -- ^ deleted
  -> Maybe LocalTime -- ^ updated
  -> CurlClientM [Institution]
getInstitutions institutionIdMaybe institutionAssociationCDMaybe institutionEncodingMaybe institutionIsForeignMaybe institutionNameMaybe institutionCreatedMaybe institutionDeletedMaybe institutionUpdatedMaybe = newCurlClientM Proxy GET "institutions" $ Data.Maybe.mapMaybe unwrapQueryParameter
  [ "_id"            .= institutionIdMaybe
  , "association_cd" .= institutionAssociationCDMaybe
  , "encoding"       .= institutionEncodingMaybe
  , "is_foreign"     .= institutionIsForeignMaybe
  , "name"           .= institutionNameMaybe
  , "created"        .= institutionCreatedMaybe
  , "deleted"        .= institutionDeletedMaybe
  , "updated"        .= institutionUpdatedMaybe
  ]
{-# INLINE  getInstitutions #-}

-- | GET /instrument_custodian?_id={_id}
--
getInstrumentCustodian
  :: ()
  => InstrumentCustodianId -- ^ _id
  -> CurlClientM (Maybe InstrumentCustodian)
getInstrumentCustodian x = fmap safeHead $ getInstrumentCustodians (Just x) Nothing Nothing Nothing Nothing Nothing
{-# INLINE  getInstrumentCustodian #-}

-- | GET /instrument_custodian
--
getInstrumentCustodians
  :: ()
  => Maybe InstrumentCustodianId -- ^ _id
  -> Maybe UserId -- ^ custodian
  -> Maybe InstrumentId -- ^ instrument
  -> Maybe LocalTime -- ^ created
  -> Maybe LocalTime -- ^ deleted
  -> Maybe LocalTime -- ^ updated
  -> CurlClientM [InstrumentCustodian]
getInstrumentCustodians instrumentCustodianIdMaybe instrumentCustodianCustodianMaybe instrumentCustodianInstrumentMaybe instrumentCustodianCreatedMaybe instrumentCustodianDeletedMaybe instrumentCustodianUpdatedMaybe = newCurlClientM Proxy GET "instrument_custodian" $ Data.Maybe.mapMaybe unwrapQueryParameter
  [ "_id"        .= instrumentCustodianIdMaybe
  , "custodian"  .= instrumentCustodianCustodianMaybe
  , "instrument" .= instrumentCustodianInstrumentMaybe
  , "created"    .= instrumentCustodianCreatedMaybe
  , "deleted"    .= instrumentCustodianDeletedMaybe
  , "updated"    .= instrumentCustodianUpdatedMaybe
  ]
{-# INLINE  getInstrumentCustodians #-}

-- | GET /journals?_id={_id}
--
getJournal
  :: ()
  => JournalId -- ^ _id
  -> CurlClientM (Maybe Journal)
getJournal x = fmap safeHead $ getJournals (Just x) Nothing Nothing Nothing Nothing Nothing Nothing Nothing
{-# INLINE  getJournal #-}

-- | GET /journals
--
getJournals
  :: ()
  => Maybe JournalId -- ^ _id
  -> Maybe Text -- ^ encoding
  -> Maybe Double -- ^ impact_factor
  -> Maybe Text -- ^ name
  -> Maybe Text -- ^ website_url
  -> Maybe LocalTime -- ^ created
  -> Maybe LocalTime -- ^ deleted
  -> Maybe LocalTime -- ^ updated
  -> CurlClientM [Journal]
getJournals journalIdMaybe journalEncodingMaybe journalImpactFactorMaybe journalNameMaybe journalWebsiteURLMaybe journalCreatedMaybe journalDeletedMaybe journalUpdatedMaybe = newCurlClientM Proxy GET "journals" $ Data.Maybe.mapMaybe unwrapQueryParameter
  [ "_id"           .= journalIdMaybe
  , "encoding"      .= journalEncodingMaybe
  , "impact_factor" .= journalImpactFactorMaybe
  , "name"          .= journalNameMaybe
  , "website_url"   .= journalWebsiteURLMaybe
  , "created"       .= journalCreatedMaybe
  , "deleted"       .= journalDeletedMaybe
  , "updated"       .= journalUpdatedMaybe
  ]
{-# INLINE  getJournals #-}

-- | GET /proposals?_id={_id}
--
getProposal
  :: ()
  => ProposalId -- ^ _id
  -> CurlClientM (Maybe Proposal)
getProposal x = fmap safeHead $ getProposals (Just x) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
{-# INLINE  getProposal #-}

-- | GET /proposals
--
getProposals
  :: ()
  => Maybe ProposalId -- ^ _id
  -> Maybe Text -- ^ abstract
  -> Maybe Day -- ^ accepted_date
  -> Maybe Day -- ^ actual_end_date
  -> Maybe Day -- ^ actual_start_date
  -> Maybe Day -- ^ closed_date
  -> Maybe Text -- ^ encoding
  -> Maybe Text -- ^ proposal_type
  -> Maybe Text -- ^ science_theme
  -> Maybe LocalTime -- ^ submitted_date
  -> Maybe Text -- ^ title
  -> Maybe LocalTime -- ^ created
  -> Maybe LocalTime -- ^ deleted
  -> Maybe LocalTime -- ^ updated
  -> CurlClientM [Proposal]
getProposals proposalIdMaybe proposalAbstractMaybe proposalAcceptedDateMaybe proposalActualEndDateMaybe proposalActualStartDateMaybe proposalClosedDateMaybe proposalEncodingMaybe proposalProposalTypeMaybe proposalScienceThemeMaybe proposalSubmittedDateMaybe proposalTitleMaybe proposalCreatedMaybe proposalDeletedMaybe proposalUpdatedMaybe = newCurlClientM Proxy GET "proposals" $ Data.Maybe.mapMaybe unwrapQueryParameter
  [ "_id"               .= proposalIdMaybe
  , "abstract"          .= proposalAbstractMaybe
  , "accepted_date"     .= proposalAcceptedDateMaybe
  , "actual_end_date"   .= proposalActualEndDateMaybe
  , "actual_start_date" .= proposalActualStartDateMaybe
  , "closed_date"       .= proposalClosedDateMaybe
  , "encoding"          .= proposalEncodingMaybe
  , "proposal_type"     .= proposalProposalTypeMaybe
  , "science_theme"     .= proposalScienceThemeMaybe
  , "submitted_date"    .= proposalSubmittedDateMaybe
  , "title"             .= proposalTitleMaybe
  , "created"           .= proposalCreatedMaybe
  , "deleted"           .= proposalDeletedMaybe
  , "updated"           .= proposalUpdatedMaybe
  ]
{-# INLINE  getProposals #-}

-- | GET /users?_id={_id}
--
getUser
  :: UserId -- ^ _id
  -> CurlClientM (Maybe User)
getUser x = fmap safeHead $ getUsers (Just x) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
{-# INLINE  getUser #-}

-- | GET /users
--
getUsers
  :: ()
  => Maybe UserId -- ^ _id
  -> Maybe Text -- ^ encoding
  -> Maybe Text -- ^ first_name
  -> Maybe Text -- ^ last_name
  -> Maybe Text -- ^ middle_initial
  -> Maybe NetworkId -- ^ network_id
  -> Maybe LocalTime -- ^ created
  -> Maybe LocalTime -- ^ deleted
  -> Maybe LocalTime -- ^ updated
  -> CurlClientM [User]
getUsers userIdMaybe userEncodingMaybe userFirstNameMaybe userLastNameMaybe userMiddleInitialMaybe userNetworkIdMaybe userCreatedMaybe userDeletedMaybe userUpdatedMaybe = newCurlClientM Proxy GET "users" $ Data.Maybe.mapMaybe unwrapQueryParameter
  [ "_id"            .= userIdMaybe
  , "encoding"       .= userEncodingMaybe
  , "first_name"     .= userFirstNameMaybe
  , "last_name"      .= userLastNameMaybe
  , "middle_initial" .= userMiddleInitialMaybe
  , "network_id"     .= userNetworkIdMaybe
  , "created"        .= userCreatedMaybe
  , "deleted"        .= userDeletedMaybe
  , "updated"        .= userUpdatedMaybe
  ]
{-# INLINE  getUsers #-}

-- | A wrapped query parameter.
--
-- Note: This type uses existential quantification to accept any type with a 'ToJSON' instance.
--
data WrappedQueryParameter = forall a. (ToJSON a) => WrapQueryParameter (Proxy a) String (Maybe a)

-- | Unwrap a wrapped query parameter, and return a key-value pair.
--
unwrapQueryParameter :: WrappedQueryParameter -> Maybe (String, String)
unwrapQueryParameter (WrapQueryParameter Proxy k mv) = fmap ((,) k . Data.ByteString.Lazy.Char8.unpack . Data.Aeson.encode) mv
{-# INLINE  unwrapQueryParameter #-}

-- | Syntactic sugar for 'WrapQueryParameter' constructor.
--
(.=) :: (ToJSON a) => String -> Maybe a -> WrappedQueryParameter
(.=) = WrapQueryParameter Proxy
{-# INLINE  (.=) #-}

-- | Implementation of 'head' function that is safe for empty lists.
--
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : _) = Just x
{-# INLINE  safeHead #-}

-- | Append an element to a list.
--
snoc :: a -> [a] -> [a]
snoc x xs = xs ++ [x]
{-# INLINE  snoc #-}
