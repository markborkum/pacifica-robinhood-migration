{-# LANGUAGE  DataKinds #-}
{-# LANGUAGE  FlexibleInstances #-}
{-# LANGUAGE  KindSignatures #-}
{-# LANGUAGE  ScopedTypeVariables #-}
{-# LANGUAGE  TypeFamilies #-}
{-# LANGUAGE  TypeOperators #-}

-- |
-- Module:      Pacifica.Metadata.API.Curl
-- Copyright:   (c) 2017 Pacific Northwest National Laboratory
-- License:     LGPL
-- Maintainer:  Mark Borkum <mark.borkum@pnnl.gov>
-- Stability:   experimental
-- Portability: portable
--
-- This module exports Haskell functions that implement the APIs for Pacifica Metadata Services using a cURL-based client.
--
-- For example, to request the data from a Pacifica Metadata Services deployment at http://0.0.0.0:8121 for the 'User' with @('UserId' 42)@:
--
-- > import Control.Monad.Logger
-- > import Data.Default
-- > import Network.Curl.Client
-- > import Network.URL
-- > import Pacifica.Metadata
-- > import Pacifica.Metadata.API.Curl
-- > import System.Process
-- >
-- > main :: IO ()
-- > main = do
-- >   let
-- >     spec :: CurlCmdSpec
-- >     spec = def
-- >     env :: CurlClientEnv
-- >     env = CurlClientEnv spec $ Absolute $ Host (HTTP False) "0.0.0.0" $ Just 8121
-- >     mx :: CurlClientT (LoggingT IO) (Maybe User)
-- >     mx = fromCurlRequest $ readUserByPrimaryKey $ UserId 42
-- >   x <- runStderrLoggingT $ runCurlClientT mx env
-- >   case x of
-- >     Left err -> putStr "Error: " >> print err
-- >     Right Nothing -> putStrLn "Warning: User not found"
-- >     Right (Just user) -> print user
--
module Pacifica.Metadata.API.Curl
  ( -- * API implementations
    -- ** AnalyticalTool type
    createAnalyticalTool , readAnalyticalTool , readAnalyticalToolByPrimaryKey , updateAnalyticalTool , destroyAnalyticalTool
    -- ** AnalyticalToolProposal type
  , createAnalyticalToolProposal , readAnalyticalToolProposal , readAnalyticalToolProposalByPrimaryKey , updateAnalyticalToolProposal , destroyAnalyticalToolProposal
    -- ** AnalyticalToolTransaction type
  , createAnalyticalToolTransaction , readAnalyticalToolTransaction , readAnalyticalToolTransactionByPrimaryKey , updateAnalyticalToolTransaction , destroyAnalyticalToolTransaction
    -- ** Citation type
  , createCitation , readCitation , readCitationByPrimaryKey , updateCitation , destroyCitation
    -- ** CitationContributor type
  , createCitationContributor , readCitationContributor , readCitationContributorByPrimaryKey , updateCitationContributor , destroyCitationContributor
    -- ** CitationKeyword type
  , createCitationKeyword , readCitationKeyword , readCitationKeywordByPrimaryKey , updateCitationKeyword , destroyCitationKeyword
    -- ** CitationProposal type
  , createCitationProposal , readCitationProposal , readCitationProposalByPrimaryKey , updateCitationProposal , destroyCitationProposal
    -- ** Contributor type
  , createContributor , readContributor , readContributorByPrimaryKey , updateContributor , destroyContributor
    -- ** File type
  , createFile , readFile , readFileByPrimaryKey , updateFile , destroyFile
    -- ** FileKeyValue type
  , createFileKeyValue , readFileKeyValue , readFileKeyValueByPrimaryKey , updateFileKeyValue , destroyFileKeyValue
    -- ** Group type
  , createGroup , readGroup , readGroupByPrimaryKey , updateGroup , destroyGroup
    -- ** Institution type
  , createInstitution , readInstitution , readInstitutionByPrimaryKey , updateInstitution , destroyInstitution
    -- ** InstitutionPerson type
  , createInstitutionPerson , readInstitutionPerson , readInstitutionPersonByPrimaryKey , updateInstitutionPerson , destroyInstitutionPerson
    -- ** Instrument type
  , createInstrument , readInstrument , readInstrumentByPrimaryKey , updateInstrument , destroyInstrument
    -- ** InstrumentCustodian type
  , createInstrumentCustodian , readInstrumentCustodian , readInstrumentCustodianByPrimaryKey , updateInstrumentCustodian , destroyInstrumentCustodian
    -- ** InstrumentGroup type
  , createInstrumentGroup , readInstrumentGroup , readInstrumentGroupByPrimaryKey , updateInstrumentGroup , destroyInstrumentGroup
    -- ** Journal type
  , createJournal , readJournal , readJournalByPrimaryKey , updateJournal , destroyJournal
    -- ** Key type
  , createKey , readKey , readKeyByPrimaryKey , updateKey , destroyKey
    -- ** Keyword type
  , createKeyword , readKeyword , readKeywordByPrimaryKey , updateKeyword , destroyKeyword
    -- ** Proposal type
  , createProposal , readProposal , readProposalByPrimaryKey , updateProposal , destroyProposal
    -- ** ProposalInstrument type
  , createProposalInstrument , readProposalInstrument , readProposalInstrumentByPrimaryKey , updateProposalInstrument , destroyProposalInstrument
    -- ** ProposalParticipant type
  , createProposalParticipant , readProposalParticipant , readProposalParticipantByPrimaryKey , updateProposalParticipant , destroyProposalParticipant
    -- ** Transaction type
  , createTransaction , readTransaction , readTransactionByPrimaryKey , updateTransaction , destroyTransaction
    -- ** TransactionKeyValue type
  , createTransactionKeyValue , readTransactionKeyValue , readTransactionKeyValueByPrimaryKey , updateTransactionKeyValue , destroyTransactionKeyValue
    -- ** User type
  , createUser , readUser , readUserByPrimaryKey , updateUser , destroyUser
    -- ** UserGroup type
  , createUserGroup , readUserGroup , readUserGroupByPrimaryKey , updateUserGroup , destroyUserGroup
    -- ** Value type
  , createValue , readValue , readValueByPrimaryKey , updateValue , destroyValue
  ) where

import           Data.Aeson (FromJSON(), ToJSON())
import qualified Data.Aeson
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8
import           Data.Default (Default(def))
import qualified Data.List
import qualified Data.List.Extra
import qualified Data.Maybe
import           Data.Proxy (Proxy(..))
import           Data.Text (Text)
import qualified Data.Text
import           Data.Time.Calendar (Day)
import           Data.Time.LocalTime (LocalTime)
import           GHC.TypeLits (KnownNat(), KnownSymbol(), Nat)
import qualified GHC.TypeLits
import           Network.Curl.Client (CurlRequest(..))
import           Network.HTTP.Types (StdMethod)
import qualified Network.HTTP.Types
import           Network.URL (URL(..))
import           Pacifica.Metadata
import           Pacifica.Metadata.API
import           Prelude hiding (readFile)
import           Servant.API.Alternative ((:<|>)(..))
import           Servant.API.ContentTypes (JSON, NoContent(NoContent))
import           Servant.API.QueryParam (QueryParam)
import           Servant.API.ReqBody (ReqBody)
import           Servant.API.Sub ((:>))
import           Servant.API.Verbs (ReflectMethod(reflectMethod), Verb)
import qualified Text.Printf
import           Web.Internal.HttpApiData (ToHttpApiData())
import qualified Web.Internal.HttpApiData

createAnalyticalTool
  :: ()
  => AnalyticalTool -- ^ DATA
  -> CurlRequest NoContent
readAnalyticalTool
  :: ()
  => Maybe AnalyticalToolId -- ^ _id
  -> Maybe Text -- ^ encoding
  -> Maybe Text -- ^ name
  -> Maybe LocalTime -- ^ created
  -> Maybe LocalTime -- ^ deleted
  -> Maybe LocalTime -- ^ updated
  -> Maybe Int -- ^ items_per_page
  -> Maybe Int -- ^ page_number
  -> CurlRequest [AnalyticalTool]
updateAnalyticalTool
  :: ()
  => Maybe AnalyticalToolId -- ^ _id
  -> AnalyticalTool -- ^ DATA
  -> CurlRequest NoContent
destroyAnalyticalTool
  :: ()
  => Maybe AnalyticalToolId -- ^ _id
  -> CurlRequest NoContent
(createAnalyticalTool :<|> readAnalyticalTool :<|> updateAnalyticalTool :<|> destroyAnalyticalTool) = toClient apiAnalyticalTool def
{-# NOINLINE  createAnalyticalTool #-}
{-# NOINLINE  readAnalyticalTool #-}
{-# NOINLINE  updateAnalyticalTool #-}
{-# NOINLINE  destroyAnalyticalTool #-}

readAnalyticalToolByPrimaryKey
  :: ()
  => AnalyticalToolId -- ^ _id
  -> CurlRequest (Maybe AnalyticalTool)
readAnalyticalToolByPrimaryKey analyticalToolId = Data.Maybe.listToMaybe <$> readAnalyticalTool (Just analyticalToolId) Nothing Nothing Nothing Nothing Nothing (Just 1) (Just 1)
{-# INLINE  readAnalyticalToolByPrimaryKey #-}

createAnalyticalToolProposal
  :: ()
  => AnalyticalToolProposal -- ^ DATA
  -> CurlRequest NoContent
readAnalyticalToolProposal
  :: ()
  => Maybe AnalyticalToolProposalId -- ^ _id
  -> Maybe AnalyticalToolId -- ^ analytical_tool_id
  -> Maybe ProposalId -- ^ proposal_id
  -> Maybe LocalTime -- ^ created
  -> Maybe LocalTime -- ^ deleted
  -> Maybe LocalTime -- ^ updated
  -> Maybe Int -- ^ items_per_page
  -> Maybe Int -- ^ page_number
  -> CurlRequest [AnalyticalToolProposal]
updateAnalyticalToolProposal
  :: ()
  => Maybe AnalyticalToolProposalId -- ^ _id
  -> AnalyticalToolProposal -- ^ DATA
  -> CurlRequest NoContent
destroyAnalyticalToolProposal
  :: ()
  => Maybe AnalyticalToolProposalId -- ^ _id
  -> CurlRequest NoContent
createAnalyticalToolProposal :<|> readAnalyticalToolProposal :<|> updateAnalyticalToolProposal :<|> destroyAnalyticalToolProposal = toClient apiAnalyticalToolProposal def
{-# NOINLINE  createAnalyticalToolProposal #-}
{-# NOINLINE  readAnalyticalToolProposal #-}
{-# NOINLINE  updateAnalyticalToolProposal #-}
{-# NOINLINE  destroyAnalyticalToolProposal #-}

readAnalyticalToolProposalByPrimaryKey
  :: ()
  => AnalyticalToolProposalId -- ^ _id
  -> CurlRequest (Maybe AnalyticalToolProposal)
readAnalyticalToolProposalByPrimaryKey analyticalToolProposalId = Data.Maybe.listToMaybe <$> readAnalyticalToolProposal (Just analyticalToolProposalId) Nothing Nothing Nothing Nothing Nothing (Just 1) (Just 1)
{-# INLINE  readAnalyticalToolProposalByPrimaryKey #-}

createAnalyticalToolTransaction
  :: ()
  => AnalyticalToolTransaction -- ^ DATA
  -> CurlRequest NoContent
readAnalyticalToolTransaction
  :: ()
  => Maybe AnalyticalToolTransactionId -- ^ _id
  -> Maybe AnalyticalToolId -- ^ analytical_tool_id
  -> Maybe TransactionId -- ^ transaction_id
  -> Maybe LocalTime -- ^ created
  -> Maybe LocalTime -- ^ deleted
  -> Maybe LocalTime -- ^ updated
  -> Maybe Int -- ^ items_per_page
  -> Maybe Int -- ^ page_number
  -> CurlRequest [AnalyticalToolTransaction]
updateAnalyticalToolTransaction
  :: ()
  => Maybe AnalyticalToolTransactionId -- ^ _id
  -> AnalyticalToolTransaction -- ^ DATA
  -> CurlRequest NoContent
destroyAnalyticalToolTransaction
  :: ()
  => Maybe AnalyticalToolTransactionId -- ^ _id
  -> CurlRequest NoContent
createAnalyticalToolTransaction :<|> readAnalyticalToolTransaction :<|> updateAnalyticalToolTransaction :<|> destroyAnalyticalToolTransaction = toClient apiAnalyticalToolTransaction def
{-# NOINLINE  createAnalyticalToolTransaction #-}
{-# NOINLINE  readAnalyticalToolTransaction #-}
{-# NOINLINE  updateAnalyticalToolTransaction #-}
{-# NOINLINE  destroyAnalyticalToolTransaction #-}

readAnalyticalToolTransactionByPrimaryKey
  :: ()
  => AnalyticalToolTransactionId -- ^ _id
  -> CurlRequest (Maybe AnalyticalToolTransaction)
readAnalyticalToolTransactionByPrimaryKey analyticalToolTransactionId = Data.Maybe.listToMaybe <$> readAnalyticalToolTransaction (Just analyticalToolTransactionId) Nothing Nothing Nothing Nothing Nothing (Just 1) (Just 1)
{-# INLINE  readAnalyticalToolTransactionByPrimaryKey #-}

createCitation
  :: ()
  => Citation -- ^ DATA
  -> CurlRequest NoContent
readCitation
  :: ()
  => Maybe CitationId -- ^ _id
  -> Maybe Text -- ^ abstract_text
  -> Maybe Text -- ^ article_title
  -> Maybe Text -- ^ doi_reference
  -> Maybe Text -- ^ encoding
  -> Maybe JournalId -- ^ journal_id
  -> Maybe Int -- ^ journal_issue
  -> Maybe Int -- ^ journal_volume
  -> Maybe Text -- ^ page_range
  -> Maybe ReleaseAuthorizationId -- ^ release_authorization_id
  -> Maybe Text -- ^ xml_text
  -> Maybe LocalTime -- ^ created
  -> Maybe LocalTime -- ^ deleted
  -> Maybe LocalTime -- ^ updated
  -> Maybe Int -- ^ items_per_page
  -> Maybe Int -- ^ page_number
  -> CurlRequest [Citation]
updateCitation
  :: ()
  => Maybe CitationId -- ^ _id
  -> Citation -- ^ DATA
  -> CurlRequest NoContent
destroyCitation
  :: ()
  => Maybe CitationId -- ^ _id
  -> CurlRequest NoContent
createCitation :<|> readCitation :<|> updateCitation :<|> destroyCitation = toClient apiCitation def
{-# NOINLINE  createCitation #-}
{-# NOINLINE  readCitation #-}
{-# NOINLINE  updateCitation #-}
{-# NOINLINE  destroyCitation #-}

readCitationByPrimaryKey
  :: ()
  => CitationId -- ^ _id
  -> CurlRequest (Maybe Citation)
readCitationByPrimaryKey citationId = Data.Maybe.listToMaybe <$> readCitation (Just citationId) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing (Just 1) (Just 1)
{-# INLINE  readCitationByPrimaryKey #-}

createCitationContributor
  :: ()
  => CitationContributor -- ^ DATA
  -> CurlRequest NoContent
readCitationContributor
  :: ()
  => Maybe CitationContributorId -- ^ _id
  -> Maybe ContributorId -- ^ author_id
  -> Maybe Int -- ^ author_precedence
  -> Maybe CitationId -- ^ citation_id
  -> Maybe LocalTime -- ^ created
  -> Maybe LocalTime -- ^ deleted
  -> Maybe LocalTime -- ^ updated
  -> Maybe Int -- ^ items_per_page
  -> Maybe Int -- ^ page_number
  -> CurlRequest [CitationContributor]
updateCitationContributor
  :: ()
  => Maybe CitationContributorId -- ^ _id
  -> CitationContributor -- ^ DATA
  -> CurlRequest NoContent
destroyCitationContributor
  :: ()
  => Maybe CitationContributorId -- ^ _id
  -> CurlRequest NoContent
createCitationContributor :<|> readCitationContributor :<|> updateCitationContributor :<|> destroyCitationContributor = toClient apiCitationContributor def
{-# NOINLINE  createCitationContributor #-}
{-# NOINLINE  readCitationContributor #-}
{-# NOINLINE  updateCitationContributor #-}
{-# NOINLINE  destroyCitationContributor #-}

readCitationContributorByPrimaryKey
  :: ()
  => CitationContributorId -- ^ _id
  -> CurlRequest (Maybe CitationContributor)
readCitationContributorByPrimaryKey citationContributorId = Data.Maybe.listToMaybe <$> readCitationContributor (Just citationContributorId) Nothing Nothing Nothing Nothing Nothing Nothing (Just 1) (Just 1)
{-# INLINE  readCitationContributorByPrimaryKey #-}

createCitationKeyword
  :: ()
  => CitationKeyword -- ^ DATA
  -> CurlRequest NoContent
readCitationKeyword
  :: ()
  => Maybe CitationKeywordId -- ^ _id
  -> Maybe CitationId -- ^ citation_id
  -> Maybe KeywordId -- ^ keyword_id
  -> Maybe LocalTime -- ^ created
  -> Maybe LocalTime -- ^ deleted
  -> Maybe LocalTime -- ^ updated
  -> Maybe Int -- ^ items_per_page
  -> Maybe Int -- ^ page_number
  -> CurlRequest [CitationKeyword]
updateCitationKeyword
  :: ()
  => Maybe CitationKeywordId -- ^ _id
  -> CitationKeyword -- ^ DATA
  -> CurlRequest NoContent
destroyCitationKeyword
  :: ()
  => Maybe CitationKeywordId -- ^ _id
  -> CurlRequest NoContent
createCitationKeyword :<|> readCitationKeyword :<|> updateCitationKeyword :<|> destroyCitationKeyword = toClient apiCitationKeyword def
{-# NOINLINE  createCitationKeyword #-}
{-# NOINLINE  readCitationKeyword #-}
{-# NOINLINE  updateCitationKeyword #-}
{-# NOINLINE  destroyCitationKeyword #-}

readCitationKeywordByPrimaryKey
  :: ()
  => CitationKeywordId -- ^ _id
  -> CurlRequest (Maybe CitationKeyword)
readCitationKeywordByPrimaryKey citationKeywordId = Data.Maybe.listToMaybe <$> readCitationKeyword (Just citationKeywordId) Nothing Nothing Nothing Nothing Nothing (Just 1) (Just 1)
{-# INLINE  readCitationKeywordByPrimaryKey #-}

createCitationProposal
  :: ()
  => CitationProposal -- ^ DATA
  -> CurlRequest NoContent
readCitationProposal
  :: ()
  => Maybe CitationProposalId -- ^ _id
  -> Maybe CitationId -- ^ citation_id
  -> Maybe ProposalId -- ^ proposal_id
  -> Maybe LocalTime -- ^ created
  -> Maybe LocalTime -- ^ deleted
  -> Maybe LocalTime -- ^ updated
  -> Maybe Int -- ^ items_per_page
  -> Maybe Int -- ^ page_number
  -> CurlRequest [CitationProposal]
updateCitationProposal
  :: ()
  => Maybe CitationProposalId -- ^ _id
  -> CitationProposal -- ^ DATA
  -> CurlRequest NoContent
destroyCitationProposal
  :: ()
  => Maybe CitationProposalId -- ^ _id
  -> CurlRequest NoContent
createCitationProposal :<|>readCitationProposal :<|> updateCitationProposal :<|> destroyCitationProposal = toClient apiCitationProposal def
{-# NOINLINE  createCitationProposal #-}
{-# NOINLINE  readCitationProposal #-}
{-# NOINLINE  updateCitationProposal #-}
{-# NOINLINE  destroyCitationProposal #-}

readCitationProposalByPrimaryKey
  :: ()
  => CitationProposalId -- ^ _id
  -> CurlRequest (Maybe CitationProposal)
readCitationProposalByPrimaryKey citationProposalId = Data.Maybe.listToMaybe <$> readCitationProposal (Just citationProposalId) Nothing Nothing Nothing Nothing Nothing (Just 1) (Just 1)
{-# INLINE  readCitationProposalByPrimaryKey #-}

createContributor
  :: ()
  => Contributor -- ^ DATA
  -> CurlRequest NoContent
readContributor
  :: ()
  => Maybe ContributorId -- ^ _id
  -> Maybe Text -- ^ dept_code
  -> Maybe Text -- ^ encoding
  -> Maybe Text -- ^ first_name
  -> Maybe InstitutionId -- ^ institution_id
  -> Maybe Text -- ^ last_name
  -> Maybe Text -- ^ middle_initial
  -> Maybe UserId -- ^ person_id
  -> Maybe LocalTime -- ^ created
  -> Maybe LocalTime -- ^ deleted
  -> Maybe LocalTime -- ^ updated
  -> Maybe Int -- ^ items_per_page
  -> Maybe Int -- ^ page_number
  -> CurlRequest [Contributor]
updateContributor
  :: ()
  => Maybe ContributorId -- ^ _id
  -> Contributor -- ^ DATA
  -> CurlRequest NoContent
destroyContributor
  :: ()
  => Maybe ContributorId -- ^ _id
  -> CurlRequest NoContent
createContributor :<|> readContributor :<|> updateContributor :<|> destroyContributor = toClient apiContributor def
{-# NOINLINE  createContributor #-}
{-# NOINLINE  readContributor #-}
{-# NOINLINE  updateContributor #-}
{-# NOINLINE  destroyContributor #-}

readContributorByPrimaryKey
  :: ()
  => ContributorId -- ^ _id
  -> CurlRequest (Maybe Contributor)
readContributorByPrimaryKey contributorId = Data.Maybe.listToMaybe <$> readContributor (Just contributorId) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing (Just 1) (Just 1)
{-# INLINE  readContributorByPrimaryKey #-}

createFile
  :: ()
  => File -- ^ DATA
  -> CurlRequest NoContent
readFile
  :: ()
  => Maybe FileId -- ^ _id
  -> Maybe LocalTime -- ^ ctime
  -> Maybe Text -- ^ encoding
  -> Maybe Text -- ^ hashsum
  -> Maybe Text -- ^ hashtype
  -> Maybe Text -- ^ mimetype
  -> Maybe LocalTime -- ^ mtime
  -> Maybe Text -- ^ name
  -> Maybe Int -- ^ size
  -> Maybe Text -- ^ subdir
  -> Maybe TransactionId -- ^ transaction_id
  -> Maybe LocalTime -- ^ created
  -> Maybe LocalTime -- ^ deleted
  -> Maybe LocalTime -- ^ updated
  -> Maybe Int -- ^ items_per_page
  -> Maybe Int -- ^ page_number
  -> CurlRequest [File]
updateFile
  :: ()
  => Maybe FileId -- ^ _id
  -> File -- ^ DATA
  -> CurlRequest NoContent
destroyFile
  :: ()
  => Maybe FileId -- ^ _id
  -> CurlRequest NoContent
createFile :<|> readFile :<|> updateFile :<|> destroyFile = toClient apiFile def
{-# NOINLINE  createFile #-}
{-# NOINLINE  readFile #-}
{-# NOINLINE  updateFile #-}
{-# NOINLINE  destroyFile #-}

readFileByPrimaryKey
  :: ()
  => FileId
  -> CurlRequest (Maybe File)
readFileByPrimaryKey fileId = Data.Maybe.listToMaybe <$> readFile (Just fileId) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing (Just 1) (Just 1)
{-# INLINE  readFileByPrimaryKey #-}

createFileKeyValue
  :: ()
  => FileKeyValue -- ^ DATA
  -> CurlRequest NoContent
readFileKeyValue
  :: ()
  => Maybe FileKeyValueId -- ^ _id
  -> Maybe FileId -- ^ file_id
  -> Maybe KeyId -- ^ key_id
  -> Maybe ValueId -- ^ value_id
  -> Maybe LocalTime -- ^ created
  -> Maybe LocalTime -- ^ deleted
  -> Maybe LocalTime -- ^ updated
  -> Maybe Int -- ^ items_per_page
  -> Maybe Int -- ^ page_number
  -> CurlRequest [FileKeyValue]
updateFileKeyValue
  :: ()
  => Maybe FileKeyValueId -- ^ _id
  -> FileKeyValue -- ^ DATA
  -> CurlRequest NoContent
destroyFileKeyValue
  :: ()
  => Maybe FileKeyValueId -- ^ _id
  -> CurlRequest NoContent
createFileKeyValue :<|> readFileKeyValue :<|> updateFileKeyValue :<|> destroyFileKeyValue = toClient apiFileKeyValue def
{-# NOINLINE  createFileKeyValue #-}
{-# NOINLINE  readFileKeyValue #-}
{-# NOINLINE  updateFileKeyValue #-}
{-# NOINLINE  destroyFileKeyValue #-}

readFileKeyValueByPrimaryKey
  :: ()
  => FileKeyValueId
  -> CurlRequest (Maybe FileKeyValue)
readFileKeyValueByPrimaryKey fileKeyValueId = Data.Maybe.listToMaybe <$> readFileKeyValue (Just fileKeyValueId) Nothing Nothing Nothing Nothing Nothing Nothing (Just 1) (Just 1)
{-# INLINE  readFileKeyValueByPrimaryKey #-}

createGroup
  :: ()
  => Group -- ^ DATA
  -> CurlRequest NoContent
readGroup
  :: ()
  => Maybe GroupId -- ^ _id
  -> Maybe Text -- ^ encoding
  -> Maybe Bool -- ^ is_admin
  -> Maybe Text -- ^ name
  -> Maybe LocalTime -- ^ created
  -> Maybe LocalTime -- ^ deleted
  -> Maybe LocalTime -- ^ updated
  -> Maybe Int -- ^ items_per_page
  -> Maybe Int -- ^ page_number
  -> CurlRequest [Group]
updateGroup
  :: ()
  => Maybe GroupId -- ^ _id
  -> Group -- ^ DATA
  -> CurlRequest NoContent
destroyGroup
  :: ()
  => Maybe GroupId -- ^ _id
  -> CurlRequest NoContent
createGroup :<|> readGroup :<|> updateGroup :<|> destroyGroup = toClient apiGroup def
{-# NOINLINE  createGroup #-}
{-# NOINLINE  readGroup #-}
{-# NOINLINE  updateGroup #-}
{-# NOINLINE  destroyGroup #-}

readGroupByPrimaryKey
  :: ()
  => GroupId
  -> CurlRequest (Maybe Group)
readGroupByPrimaryKey groupId = Data.Maybe.listToMaybe <$> readGroup (Just groupId) Nothing Nothing Nothing Nothing Nothing Nothing (Just 1) (Just 1)
{-# INLINE  readGroupByPrimaryKey #-}

createInstitution
  :: ()
  => Institution -- ^ DATA
  -> CurlRequest NoContent
readInstitution
  :: ()
  => Maybe InstitutionId -- ^ _id
  -> Maybe Text -- ^ association_cd
  -> Maybe Text -- ^ encoding
  -> Maybe Bool -- ^ is_foreign
  -> Maybe Text -- ^ name
  -> Maybe LocalTime -- ^ created
  -> Maybe LocalTime -- ^ deleted
  -> Maybe LocalTime -- ^ updated
  -> Maybe Int -- ^ items_per_page
  -> Maybe Int -- ^ page_number
  -> CurlRequest [Institution]
updateInstitution
  :: ()
  => Maybe InstitutionId -- ^ _id
  -> Institution -- ^ DATA
  -> CurlRequest NoContent
destroyInstitution
  :: ()
  => Maybe InstitutionId -- ^ _id
  -> CurlRequest NoContent
createInstitution :<|> readInstitution :<|> updateInstitution :<|> destroyInstitution = toClient apiInstitution def
{-# NOINLINE  createInstitution #-}
{-# NOINLINE  readInstitution #-}
{-# NOINLINE  updateInstitution #-}
{-# NOINLINE  destroyInstitution #-}

readInstitutionByPrimaryKey
  :: ()
  => InstitutionId
  -> CurlRequest (Maybe Institution)
readInstitutionByPrimaryKey institutionId = Data.Maybe.listToMaybe <$> readInstitution (Just institutionId) Nothing Nothing Nothing Nothing Nothing Nothing Nothing (Just 1) (Just 1)
{-# INLINE readInstitutionByPrimaryKey #-}

createInstitutionPerson
  :: ()
  => InstitutionPerson -- ^ DATA
  -> CurlRequest NoContent
readInstitutionPerson
  :: ()
  => Maybe InstitutionPersonId -- ^ _id
  -> Maybe InstitutionId -- ^ institution_id
  -> Maybe UserId -- ^ person_id
  -> Maybe LocalTime -- ^ created
  -> Maybe LocalTime -- ^ deleted
  -> Maybe LocalTime -- ^ updated
  -> Maybe Int -- ^ items_per_page
  -> Maybe Int -- ^ page_number
  -> CurlRequest [InstitutionPerson]
updateInstitutionPerson
  :: ()
  => Maybe InstitutionPersonId -- ^ _id
  -> InstitutionPerson -- ^ DATA
  -> CurlRequest NoContent
destroyInstitutionPerson
  :: ()
  => Maybe InstitutionPersonId -- ^ _id
  -> CurlRequest NoContent
createInstitutionPerson :<|> readInstitutionPerson :<|> updateInstitutionPerson :<|> destroyInstitutionPerson = toClient apiInstitutionPerson def
{-# NOINLINE  createInstitutionPerson #-}
{-# NOINLINE  readInstitutionPerson #-}
{-# NOINLINE  updateInstitutionPerson #-}
{-# NOINLINE  destroyInstitutionPerson #-}

readInstitutionPersonByPrimaryKey
  :: ()
  => InstitutionPersonId
  -> CurlRequest (Maybe InstitutionPerson)
readInstitutionPersonByPrimaryKey institutionPersonId = Data.Maybe.listToMaybe <$> readInstitutionPerson (Just institutionPersonId) Nothing Nothing Nothing Nothing Nothing (Just 1) (Just 1)
{-# INLINE  readInstitutionPersonByPrimaryKey #-}

createInstrument
  :: ()
  => Instrument -- ^ DATA
  -> CurlRequest NoContent
readInstrument
  :: ()
  => Maybe InstrumentId -- ^ _id
  -> Maybe Bool -- ^ active
  -> Maybe Text -- ^ display_name
  -> Maybe Text -- ^ encoding
  -> Maybe Text -- ^ name
  -> Maybe Text -- ^ name_short
  -> Maybe LocalTime -- ^ created
  -> Maybe LocalTime -- ^ deleted
  -> Maybe LocalTime -- ^ updated
  -> Maybe Int -- ^ items_per_page
  -> Maybe Int -- ^ page_number
  -> CurlRequest [Instrument]
updateInstrument
  :: ()
  => Maybe InstrumentId -- ^ _id
  -> Instrument -- ^ DATA
  -> CurlRequest NoContent
destroyInstrument
  :: ()
  => Maybe InstrumentId -- ^ _id
  -> CurlRequest NoContent
createInstrument :<|> readInstrument :<|> updateInstrument :<|> destroyInstrument = toClient apiInstrument def
{-# NOINLINE  createInstrument #-}
{-# NOINLINE  readInstrument #-}
{-# NOINLINE  updateInstrument #-}
{-# NOINLINE  destroyInstrument #-}

readInstrumentByPrimaryKey
  :: ()
  => InstrumentId
  -> CurlRequest (Maybe Instrument)
readInstrumentByPrimaryKey instrumentId = Data.Maybe.listToMaybe <$> readInstrument (Just instrumentId) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing (Just 1) (Just 1)
{-# INLINE  readInstrumentByPrimaryKey #-}

createInstrumentCustodian
  :: ()
  => InstrumentCustodian -- ^ DATA
  -> CurlRequest NoContent
readInstrumentCustodian
  :: ()
  => Maybe InstrumentCustodianId -- ^ _id
  -> Maybe UserId -- ^ custodian_id
  -> Maybe InstrumentId -- ^ instrument_id
  -> Maybe LocalTime -- ^ created
  -> Maybe LocalTime -- ^ deleted
  -> Maybe LocalTime -- ^ updated
  -> Maybe Int -- ^ items_per_page
  -> Maybe Int -- ^ page_number
  -> CurlRequest [InstrumentCustodian]
updateInstrumentCustodian
  :: ()
  => Maybe InstrumentCustodianId -- ^ _id
  -> InstrumentCustodian -- ^ DATA
  -> CurlRequest NoContent
destroyInstrumentCustodian
  :: ()
  => Maybe InstrumentCustodianId -- ^ _id
  -> CurlRequest NoContent
createInstrumentCustodian :<|> readInstrumentCustodian :<|> updateInstrumentCustodian :<|> destroyInstrumentCustodian = toClient apiInstrumentCustodian def
{-# NOINLINE  createInstrumentCustodian #-}
{-# NOINLINE  readInstrumentCustodian #-}
{-# NOINLINE  updateInstrumentCustodian #-}
{-# NOINLINE  destroyInstrumentCustodian #-}

readInstrumentCustodianByPrimaryKey
  :: ()
  => InstrumentCustodianId
  -> CurlRequest (Maybe InstrumentCustodian)
readInstrumentCustodianByPrimaryKey instrumentCustodianId = Data.Maybe.listToMaybe <$> readInstrumentCustodian (Just instrumentCustodianId) Nothing Nothing Nothing Nothing Nothing (Just 1) (Just 1)
{-# INLINE  readInstrumentCustodianByPrimaryKey #-}

createInstrumentGroup
  :: ()
  => InstrumentGroup -- ^ DATA
  -> CurlRequest NoContent
readInstrumentGroup
  :: ()
  => Maybe InstrumentGroupId -- ^ _id
  -> Maybe GroupId -- ^ group_id
  -> Maybe InstrumentId -- ^ instrument_id
  -> Maybe LocalTime -- ^ created
  -> Maybe LocalTime -- ^ deleted
  -> Maybe LocalTime -- ^ updated
  -> Maybe Int -- ^ items_per_page
  -> Maybe Int -- ^ page_number
  -> CurlRequest [InstrumentGroup]
updateInstrumentGroup
  :: ()
  => Maybe InstrumentGroupId -- ^ _id
  -> InstrumentGroup -- ^ DATA
  -> CurlRequest NoContent
destroyInstrumentGroup
  :: ()
  => Maybe InstrumentGroupId -- ^ _id
  -> CurlRequest NoContent
createInstrumentGroup :<|> readInstrumentGroup :<|> updateInstrumentGroup :<|> destroyInstrumentGroup = toClient apiInstrumentGroup def
{-# NOINLINE  createInstrumentGroup #-}
{-# NOINLINE  readInstrumentGroup #-}
{-# NOINLINE  updateInstrumentGroup #-}
{-# NOINLINE  destroyInstrumentGroup #-}

readInstrumentGroupByPrimaryKey
  :: ()
  => InstrumentGroupId
  -> CurlRequest (Maybe InstrumentGroup)
readInstrumentGroupByPrimaryKey instrumentGroupId = Data.Maybe.listToMaybe <$> readInstrumentGroup (Just instrumentGroupId) Nothing Nothing Nothing Nothing Nothing (Just 1) (Just 1)
{-# INLINE  readInstrumentGroupByPrimaryKey #-}

createJournal
  :: ()
  => Journal -- ^ DATA
  -> CurlRequest NoContent
readJournal
  :: ()
  => Maybe JournalId -- ^ _id
  -> Maybe Text -- ^ encoding
  -> Maybe Double -- ^ impact_factor
  -> Maybe Text -- ^ name
  -> Maybe Text -- ^ website_url
  -> Maybe LocalTime -- ^ created
  -> Maybe LocalTime -- ^ deleted
  -> Maybe LocalTime -- ^ updated
  -> Maybe Int -- ^ items_per_page
  -> Maybe Int -- ^ page_number
  -> CurlRequest [Journal]
updateJournal
  :: ()
  => Maybe JournalId -- ^ _id
  -> Journal -- ^ DATA
  -> CurlRequest NoContent
destroyJournal
  :: ()
  => Maybe JournalId -- ^ _id
  -> CurlRequest NoContent
createJournal :<|> readJournal :<|> updateJournal :<|> destroyJournal = toClient apiJournal def
{-# NOINLINE  createJournal #-}
{-# NOINLINE  readJournal #-}
{-# NOINLINE  updateJournal #-}
{-# NOINLINE  destroyJournal #-}

readJournalByPrimaryKey
  :: ()
  => JournalId
  -> CurlRequest (Maybe Journal)
readJournalByPrimaryKey journalId = Data.Maybe.listToMaybe <$> readJournal (Just journalId) Nothing Nothing Nothing Nothing Nothing Nothing Nothing (Just 1) (Just 1)
{-# INLINE  readJournalByPrimaryKey #-}

createKey
  :: ()
  => Key -- ^ DATA
  -> CurlRequest NoContent
readKey
  :: ()
  => Maybe KeyId -- ^ _id
  -> Maybe Text -- ^ encoding
  -> Maybe Text -- ^ key
  -> Maybe LocalTime -- ^ created
  -> Maybe LocalTime -- ^ deleted
  -> Maybe LocalTime -- ^ updated
  -> Maybe Int -- ^ items_per_page
  -> Maybe Int -- ^ page_number
  -> CurlRequest [Key]
updateKey
  :: ()
  => Maybe KeyId -- ^ _id
  -> Key -- ^ DATA
  -> CurlRequest NoContent
destroyKey
  :: ()
  => Maybe KeyId -- ^ _id
  -> CurlRequest NoContent
createKey :<|> readKey :<|> updateKey :<|> destroyKey = toClient apiKey def
{-# NOINLINE  createKey #-}
{-# NOINLINE  readKey #-}
{-# NOINLINE  updateKey #-}
{-# NOINLINE  destroyKey #-}

readKeyByPrimaryKey
  :: ()
  => KeyId
  -> CurlRequest (Maybe Key)
readKeyByPrimaryKey keyId = Data.Maybe.listToMaybe <$> readKey (Just keyId) Nothing Nothing Nothing Nothing Nothing (Just 1) (Just 1)
{-# INLINE  readKeyByPrimaryKey #-}

createKeyword
  :: ()
  => Keyword -- ^ DATA
  -> CurlRequest NoContent
readKeyword
  :: ()
  => Maybe KeywordId -- ^ _id
  -> Maybe Text -- ^ encoding
  -> Maybe Text -- ^ keyword
  -> Maybe LocalTime -- ^ created
  -> Maybe LocalTime -- ^ deleted
  -> Maybe LocalTime -- ^ updated
  -> Maybe Int -- ^ items_per_page
  -> Maybe Int -- ^ page_number
  -> CurlRequest [Keyword]
updateKeyword
  :: ()
  => Maybe KeywordId -- ^ _id
  -> Keyword -- ^ DATA
  -> CurlRequest NoContent
destroyKeyword
  :: ()
  => Maybe KeywordId -- ^ _id
  -> CurlRequest NoContent
createKeyword :<|> readKeyword :<|> updateKeyword :<|> destroyKeyword = toClient apiKeyword def
{-# NOINLINE  createKeyword #-}
{-# NOINLINE  readKeyword #-}
{-# NOINLINE  updateKeyword #-}
{-# NOINLINE  destroyKeyword #-}

readKeywordByPrimaryKey
  :: ()
  => KeywordId
  -> CurlRequest (Maybe Keyword)
readKeywordByPrimaryKey keywordId = Data.Maybe.listToMaybe <$> readKeyword (Just keywordId) Nothing Nothing Nothing Nothing Nothing (Just 1) (Just 1)
{-# INLINE  readKeywordByPrimaryKey #-}

createProposal
  :: ()
  => Proposal -- ^ DATA
  -> CurlRequest NoContent
readProposal
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
  -> Maybe Int -- ^ items_per_page
  -> Maybe Int -- ^ page_number
  -> CurlRequest [Proposal]
updateProposal
  :: ()
  => Maybe ProposalId -- ^ _id
  -> Proposal -- ^ DATA
  -> CurlRequest NoContent
destroyProposal
  :: ()
  => Maybe ProposalId -- ^ _id
  -> CurlRequest NoContent
createProposal :<|> readProposal :<|> updateProposal :<|> destroyProposal = toClient apiProposal def
{-# NOINLINE  createProposal #-}
{-# NOINLINE  readProposal #-}
{-# NOINLINE  updateProposal #-}
{-# NOINLINE  destroyProposal #-}

readProposalByPrimaryKey
  :: ()
  => ProposalId -- ^ _id
  -> CurlRequest (Maybe Proposal)
readProposalByPrimaryKey proposalId = Data.Maybe.listToMaybe <$> readProposal (Just proposalId) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing (Just 1) (Just 1)
{-# INLINE  readProposalByPrimaryKey #-}

createProposalInstrument
  :: ()
  => ProposalInstrument -- ^ DATA
  -> CurlRequest NoContent
readProposalInstrument
  :: ()
  => Maybe ProposalInstrumentId -- ^ _id
  -> Maybe InstrumentId -- ^ instrument_id
  -> Maybe ProposalId -- ^ proposal_id
  -> Maybe LocalTime -- ^ created
  -> Maybe LocalTime -- ^ deleted
  -> Maybe LocalTime -- ^ updated
  -> Maybe Int -- ^ items_per_page
  -> Maybe Int -- ^ page_number
  -> CurlRequest [ProposalInstrument]
updateProposalInstrument
  :: ()
  => Maybe ProposalInstrumentId -- ^ _id
  -> ProposalInstrument -- ^ DATA
  -> CurlRequest NoContent
destroyProposalInstrument
  :: ()
  => Maybe ProposalInstrumentId -- ^ _id
  -> CurlRequest NoContent
createProposalInstrument :<|> readProposalInstrument :<|> updateProposalInstrument :<|> destroyProposalInstrument = toClient apiProposalInstrument def
{-# NOINLINE  createProposalInstrument #-}
{-# NOINLINE  readProposalInstrument #-}
{-# NOINLINE  updateProposalInstrument #-}
{-# NOINLINE  destroyProposalInstrument #-}

readProposalInstrumentByPrimaryKey
  :: ()
  => ProposalInstrumentId
  -> CurlRequest (Maybe ProposalInstrument)
readProposalInstrumentByPrimaryKey proposalInstrumentId = Data.Maybe.listToMaybe <$> readProposalInstrument (Just proposalInstrumentId) Nothing Nothing Nothing Nothing Nothing (Just 1) (Just 1)
{-# INLINE  readProposalInstrumentByPrimaryKey #-}

createProposalParticipant
  :: ()
  => ProposalParticipant -- ^ DATA
  -> CurlRequest NoContent
readProposalParticipant
  :: ()
  => Maybe ProposalParticipantId -- ^ _id
  -> Maybe UserId -- ^ person_id
  -> Maybe ProposalId -- ^ proposal_id
  -> Maybe LocalTime -- ^ created
  -> Maybe LocalTime -- ^ deleted
  -> Maybe LocalTime -- ^ updated
  -> Maybe Int -- ^ items_per_page
  -> Maybe Int -- ^ page_number
  -> CurlRequest [ProposalParticipant]
updateProposalParticipant
  :: ()
  => Maybe ProposalParticipantId -- ^ _id
  -> ProposalParticipant -- ^ DATA
  -> CurlRequest NoContent
destroyProposalParticipant
  :: ()
  => Maybe ProposalParticipantId -- ^ _id
  -> CurlRequest NoContent
createProposalParticipant :<|> readProposalParticipant :<|> updateProposalParticipant :<|> destroyProposalParticipant = toClient apiProposalParticipant def
{-# NOINLINE  createProposalParticipant #-}
{-# NOINLINE  readProposalParticipant #-}
{-# NOINLINE  updateProposalParticipant #-}
{-# NOINLINE  destroyProposalParticipant #-}

readProposalParticipantByPrimaryKey
  :: ()
  => ProposalParticipantId
  -> CurlRequest (Maybe ProposalParticipant)
readProposalParticipantByPrimaryKey proposalParticipantId = Data.Maybe.listToMaybe <$> readProposalParticipant (Just proposalParticipantId) Nothing Nothing Nothing Nothing Nothing (Just 1) (Just 1)
{-# INLINE  readProposalParticipantByPrimaryKey #-}

createTransaction
  :: ()
  => Transaction -- ^ DATA
  -> CurlRequest NoContent
readTransaction
  :: ()
  => Maybe TransactionId -- ^ _id
  -> Maybe InstrumentId -- ^ instrument_id
  -> Maybe ProposalId -- ^ proposal_id
  -> Maybe UserId -- ^ submitter_id
  -> Maybe LocalTime -- ^ created
  -> Maybe LocalTime -- ^ deleted
  -> Maybe LocalTime -- ^ updated
  -> Maybe Int -- ^ items_per_page
  -> Maybe Int -- ^ page_number
  -> CurlRequest [Transaction]
updateTransaction
  :: ()
  => Maybe TransactionId -- ^ _id
  -> Transaction -- ^ DATA
  -> CurlRequest NoContent
destroyTransaction
  :: ()
  => Maybe TransactionId -- ^ _id
  -> CurlRequest NoContent
createTransaction :<|> readTransaction :<|> updateTransaction :<|> destroyTransaction = toClient apiTransaction def
{-# NOINLINE  createTransaction #-}
{-# NOINLINE  readTransaction #-}
{-# NOINLINE  updateTransaction #-}
{-# NOINLINE  destroyTransaction #-}

readTransactionByPrimaryKey
  :: ()
  => TransactionId
  -> CurlRequest (Maybe Transaction)
readTransactionByPrimaryKey transactionId = Data.Maybe.listToMaybe <$> readTransaction (Just transactionId) Nothing Nothing Nothing Nothing Nothing Nothing (Just 1) (Just 1)
{-# INLINE  readTransactionByPrimaryKey #-}

createTransactionKeyValue
  :: ()
  => TransactionKeyValue -- ^ DATA
  -> CurlRequest NoContent
readTransactionKeyValue
  :: ()
  => Maybe TransactionKeyValueId -- ^ _id
  -> Maybe KeyId -- ^ key_id
  -> Maybe TransactionId -- ^ transaction_id
  -> Maybe ValueId -- ^ value_id
  -> Maybe LocalTime -- ^ created
  -> Maybe LocalTime -- ^ deleted
  -> Maybe LocalTime -- ^ updated
  -> Maybe Int -- ^ items_per_page
  -> Maybe Int -- ^ page_number
  -> CurlRequest [TransactionKeyValue]
updateTransactionKeyValue
  :: ()
  => Maybe TransactionKeyValueId -- ^ _id
  -> TransactionKeyValue -- ^ DATA
  -> CurlRequest NoContent
destroyTransactionKeyValue
  :: ()
  => Maybe TransactionKeyValueId -- ^ _id
  -> CurlRequest NoContent
createTransactionKeyValue :<|> readTransactionKeyValue :<|> updateTransactionKeyValue :<|> destroyTransactionKeyValue = toClient apiTransactionKeyValue def
{-# NOINLINE  createTransactionKeyValue #-}
{-# NOINLINE  readTransactionKeyValue #-}
{-# NOINLINE  updateTransactionKeyValue #-}
{-# NOINLINE  destroyTransactionKeyValue #-}

readTransactionKeyValueByPrimaryKey
  :: ()
  => TransactionKeyValueId
  -> CurlRequest (Maybe TransactionKeyValue)
readTransactionKeyValueByPrimaryKey transactionKeyValueId = Data.Maybe.listToMaybe <$> readTransactionKeyValue (Just transactionKeyValueId) Nothing Nothing Nothing Nothing Nothing Nothing (Just 1) (Just 1)
{-# INLINE  readTransactionKeyValueByPrimaryKey #-}

createUser
  :: ()
  => User -- ^ DATA
  -> CurlRequest NoContent
readUser
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
  -> Maybe Int -- ^ items_per_page
  -> Maybe Int -- ^ per_page
  -> CurlRequest [User]
updateUser
  :: ()
  => Maybe UserId -- ^ _id
  -> User -- ^ DATA
  -> CurlRequest NoContent
destroyUser
  :: ()
  => Maybe UserId -- ^ _id
  -> CurlRequest NoContent
createUser :<|> readUser :<|> updateUser :<|> destroyUser = toClient apiUser def
{-# NOINLINE  createUser #-}
{-# NOINLINE  readUser #-}
{-# NOINLINE  updateUser #-}
{-# NOINLINE  destroyUser #-}

readUserByPrimaryKey
  :: ()
  => UserId -- ^ _id
  -> CurlRequest (Maybe User)
readUserByPrimaryKey userId = Data.Maybe.listToMaybe <$> readUser (Just userId) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing (Just 1) (Just 1)
{-# INLINE  readUserByPrimaryKey #-}

createUserGroup
  :: ()
  => UserGroup -- ^ DATA
  -> CurlRequest NoContent
readUserGroup
  :: ()
  => Maybe UserGroupId -- ^ _id
  -> Maybe GroupId -- ^ group_id
  -> Maybe UserId -- ^ person_id
  -> Maybe LocalTime -- ^ created
  -> Maybe LocalTime -- ^ deleted
  -> Maybe LocalTime -- ^ updated
  -> Maybe Int -- ^ items_per_page
  -> Maybe Int -- ^ per_page
  -> CurlRequest [UserGroup]
updateUserGroup
  :: ()
  => Maybe UserGroupId -- ^ _id
  -> UserGroup -- ^ DATA
  -> CurlRequest NoContent
destroyUserGroup
  :: ()
  => Maybe UserGroupId -- ^ _id
  -> CurlRequest NoContent
createUserGroup :<|> readUserGroup :<|> updateUserGroup :<|> destroyUserGroup = toClient apiUserGroup def
{-# NOINLINE  createUserGroup #-}
{-# NOINLINE  readUserGroup #-}
{-# NOINLINE  updateUserGroup #-}
{-# NOINLINE  destroyUserGroup #-}

readUserGroupByPrimaryKey
  :: ()
  => UserGroupId -- ^ _id
  -> CurlRequest (Maybe UserGroup)
readUserGroupByPrimaryKey userGroupId = Data.Maybe.listToMaybe <$> readUserGroup (Just userGroupId) Nothing Nothing Nothing Nothing Nothing (Just 1) (Just 1)
{-# INLINE  readUserGroupByPrimaryKey #-}

createValue
  :: ()
  => Value -- ^ DATA
  -> CurlRequest NoContent
readValue
  :: ()
  => Maybe ValueId -- ^ _id
  -> Maybe Text -- ^ encoding
  -> Maybe Text -- ^ value
  -> Maybe LocalTime -- ^ created
  -> Maybe LocalTime -- ^ deleted
  -> Maybe LocalTime -- ^ updated
  -> Maybe Int -- ^ items_per_page
  -> Maybe Int -- ^ per_page
  -> CurlRequest [Value]
updateValue
  :: ()
  => Maybe ValueId -- ^ _id
  -> Value -- ^ DATA
  -> CurlRequest NoContent
destroyValue
  :: ()
  => Maybe ValueId -- ^ _id
  -> CurlRequest NoContent
createValue :<|> readValue :<|> updateValue :<|> destroyValue = toClient apiValue def
{-# NOINLINE  createValue #-}
{-# NOINLINE  readValue #-}
{-# NOINLINE  updateValue #-}
{-# NOINLINE  destroyValue #-}

readValueByPrimaryKey
  :: ()
  => ValueId -- ^ _id
  -> CurlRequest (Maybe Value)
readValueByPrimaryKey valueId = Data.Maybe.listToMaybe <$> readValue (Just valueId) Nothing Nothing Nothing Nothing Nothing (Just 1) (Just 1)
{-# INLINE  readValueByPrimaryKey #-}

-- | A link to an endpoint that is described by a Servant API type.
--
data Link = Link
  { _linkSegments :: [String] -- ^ Path segments (in order).
  , _linkQueryParams :: [(String, String)] -- ^ Query parameters.
  , _linkRequestMethod :: Maybe StdMethod -- ^ HTTP request method.
  , _linkRequestBody :: Maybe ByteString -- ^ HTTP request body.
  , _linkRequestStatusCode :: Maybe Integer -- ^ HTTP response status code.
  } deriving (Eq, Ord, Read, Show)

-- | Empty HTTP GET request to root endpoint, i.e., no segments, no query parameters, and no request body.
--
instance Default Link where
  def = Link
    { _linkSegments = []
    , _linkQueryParams = []
    , _linkRequestMethod = Nothing
    , _linkRequestBody = Nothing
    , _linkRequestStatusCode = Nothing
    }
  {-# INLINE  def #-}

-- | Add a query parameter.
--
addQueryParam :: String -> String -> Link -> Link
addQueryParam k v l = l { _linkQueryParams = Data.List.Extra.snoc (_linkQueryParams l) (k, v) }
{-# INLINE  addQueryParam #-}

-- | Add a path segment.
--
addSegment :: String -> Link -> Link
addSegment seg l = l { _linkSegments = Data.List.Extra.snoc (_linkSegments l) seg }
{-# INLINE  addSegment #-}

-- | Set the HTTP request body.
--
setRequestBody :: ByteString -> Link -> Link
setRequestBody bs l = l { _linkRequestBody = Just bs }
{-# INLINE  setRequestBody #-}

-- | Set the HTTP request method.
--
setRequestMethod :: (ReflectMethod method) => Proxy (method :: StdMethod) -> Link -> Link
setRequestMethod proxy l = l { _linkRequestMethod = Just stdMethod }
  where
    stdMethod :: StdMethod
    stdMethod = either (error . Text.Printf.printf "ReflectMethod.reflectMethod failed: \"%s\"" . Data.ByteString.Lazy.Char8.unpack . Data.ByteString.Lazy.Char8.fromStrict) id $ Network.HTTP.Types.parseMethod $ reflectMethod proxy
{-# INLINE  setRequestMethod #-}

-- | Set the HTTP response status code.
--
setResponseStatusCode :: Integer -> Link -> Link
setResponseStatusCode n l = l { _linkRequestStatusCode = Just n }
{-# INLINE  setResponseStatusCode #-}

-- | Run a 'Link' with the specified decoder function.
--
runLinkWith :: (ByteString -> Either String a) -> Link -> CurlRequest a
runLinkWith eitherDecode l = CurlRequest
  { _curlRequestBody = _linkRequestBody l
  , _curlRequestMethod = _linkRequestMethod l
  , _curlRequestStatusCode = _linkRequestStatusCode l
  , _curlRequestMkURL = \url_type0 -> pure (URL url_type0) <*> (Data.List.intercalate "/" . _linkSegments) <*> _linkQueryParams $ l
  , _curlRequestDecode = eitherDecode
  }
{-# INLINE  runLinkWith #-}

-- | A class for types that can be converted to client types.
--
class HasClient endpoint where
  {-# MINIMAL toClient #-}
  -- | The client type.
  --
  type MkClient endpoint
  -- | Convert the specified endpoint and link to a client.
  --
  toClient :: Proxy endpoint -> Link -> MkClient endpoint

instance (HasClient a, HasClient b) => HasClient (a :<|> b) where
  type MkClient (a :<|> b) = MkClient a :<|> MkClient b
  toClient (Proxy :: Proxy (a :<|> b)) = pure (:<|>) <*> toClient (Proxy :: Proxy a) <*> toClient (Proxy :: Proxy b)
  {-# INLINE  toClient #-}

instance (KnownSymbol sym, HasClient sub) => HasClient (sym :> sub) where
  type MkClient (sym :> sub) = MkClient sub
  toClient (Proxy :: Proxy (sym :> sub)) = toClient (Proxy :: Proxy sub) . addSegment k
    where
      k :: String
      k = GHC.TypeLits.symbolVal (Proxy :: Proxy sym)
  {-# INLINE  toClient #-}

instance (KnownSymbol sym, ToHttpApiData v, HasClient sub) => HasClient (QueryParam sym v :> sub) where
  type MkClient (QueryParam sym v :> sub) = Maybe v -> MkClient sub
  toClient (Proxy :: Proxy (QueryParam sym a :> sub)) l vMaybe = toClient (Proxy :: Proxy sub) $ maybe id (addQueryParam k . Data.Text.unpack . Web.Internal.HttpApiData.toQueryParam) vMaybe l
    where
      k :: String
      k = GHC.TypeLits.symbolVal (Proxy :: Proxy sym)
  {-# INLINE  toClient #-}

instance (ToJSON a, HasClient sub) => HasClient (ReqBody '[JSON] a :> sub) where
  type MkClient (ReqBody '[JSON] a :> sub) = a -> MkClient sub
  toClient (Proxy :: Proxy (ReqBody '[JSON] a :> sub)) l x = toClient (Proxy :: Proxy sub) $ setRequestBody (Data.Aeson.encode x) l
  {-# INLINE  toClient #-}

instance {-# OVERLAPPING #-} (ReflectMethod method, KnownNat statusCode) => HasClient (Verb (method :: StdMethod) (statusCode :: Nat) '[JSON] NoContent) where
  type MkClient (Verb method statusCode '[JSON] NoContent) = CurlRequest NoContent
  toClient (Proxy :: Proxy (Verb method statusCode '[JSON] NoContent)) = runLinkWith (const $ Right NoContent) . setResponseStatusCode statusCode . setRequestMethod (Proxy :: Proxy method)
    where
      statusCode :: Integer
      statusCode = GHC.TypeLits.natVal (Proxy :: Proxy statusCode)
  {-# INLINE  toClient #-}

instance {-# OVERLAPPABLE #-} (ReflectMethod method, KnownNat statusCode, FromJSON a) => HasClient (Verb (method :: StdMethod) (statusCode :: Nat) '[JSON] a) where
  type MkClient (Verb method statusCode '[JSON] a) = CurlRequest a
  toClient (Proxy :: Proxy (Verb method statusCode '[JSON] a)) = runLinkWith Data.Aeson.eitherDecode' . setResponseStatusCode statusCode . setRequestMethod (Proxy :: Proxy method)
    where
      statusCode :: Integer
      statusCode = GHC.TypeLits.natVal (Proxy :: Proxy statusCode)
  {-# INLINE  toClient #-}
