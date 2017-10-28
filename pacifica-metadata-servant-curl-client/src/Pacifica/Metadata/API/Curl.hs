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
-- >     mx :: CurlClientM (Maybe User)
-- >     mx = readUserByPrimaryKey $ UserId 42
-- >   x <- runCurlClientM mx env
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
import           Data.Proxy (Proxy(..))
import           Data.Text (Text)
import qualified Data.Text
import           Data.Time.Calendar (Day)
import           Data.Time.LocalTime (LocalTime)
import           GHC.TypeLits (KnownNat(), KnownSymbol(), Nat)
import qualified GHC.TypeLits
import           Network.Curl.Client (CurlClientM, CurlRequest(..))
import qualified Network.Curl.Client
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
  -> CurlClientM NoContent
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
  -> CurlClientM [AnalyticalTool]
updateAnalyticalTool
  :: ()
  => Maybe AnalyticalToolId -- ^ _id
  -> AnalyticalTool -- ^ DATA
  -> CurlClientM NoContent
destroyAnalyticalTool
  :: ()
  => Maybe AnalyticalToolId -- ^ _id
  -> CurlClientM NoContent
(createAnalyticalTool :<|> readAnalyticalTool :<|> updateAnalyticalTool :<|> destroyAnalyticalTool) = toClient apiAnalyticalTool def
{-# NOINLINE  createAnalyticalTool #-}
{-# NOINLINE  readAnalyticalTool #-}
{-# NOINLINE  updateAnalyticalTool #-}
{-# NOINLINE  destroyAnalyticalTool #-}

readAnalyticalToolByPrimaryKey
  :: ()
  => AnalyticalToolId -- ^ _id
  -> CurlClientM (Maybe AnalyticalTool)
readAnalyticalToolByPrimaryKey analyticalToolId = safeHead <$> readAnalyticalTool (Just analyticalToolId) Nothing Nothing Nothing Nothing Nothing (Just 1) (Just 1)
{-# INLINE  readAnalyticalToolByPrimaryKey #-}

createAnalyticalToolProposal
  :: ()
  => AnalyticalToolProposal -- ^ DATA
  -> CurlClientM NoContent
readAnalyticalToolProposal
  :: ()
  => Maybe AnalyticalToolProposalId -- ^ _id
  -> Maybe AnalyticalToolId -- ^ analytic_tool_id
  -> Maybe ProposalId -- ^ proposal_id
  -> Maybe LocalTime -- ^ created
  -> Maybe LocalTime -- ^ deleted
  -> Maybe LocalTime -- ^ updated
  -> Maybe Int -- ^ items_per_page
  -> Maybe Int -- ^ page_number
  -> CurlClientM [AnalyticalToolProposal]
updateAnalyticalToolProposal
  :: ()
  => Maybe AnalyticalToolProposalId -- ^ _id
  -> AnalyticalToolProposal -- ^ DATA
  -> CurlClientM NoContent
destroyAnalyticalToolProposal
  :: ()
  => Maybe AnalyticalToolProposalId -- ^ _id
  -> CurlClientM NoContent
createAnalyticalToolProposal :<|> readAnalyticalToolProposal :<|> updateAnalyticalToolProposal :<|> destroyAnalyticalToolProposal = toClient apiAnalyticalToolProposal def
{-# NOINLINE  createAnalyticalToolProposal #-}
{-# NOINLINE  readAnalyticalToolProposal #-}
{-# NOINLINE  updateAnalyticalToolProposal #-}
{-# NOINLINE  destroyAnalyticalToolProposal #-}

readAnalyticalToolProposalByPrimaryKey
  :: ()
  => AnalyticalToolProposalId -- ^ _id
  -> CurlClientM (Maybe AnalyticalToolProposal)
readAnalyticalToolProposalByPrimaryKey analyticalToolProposalId = safeHead <$> readAnalyticalToolProposal (Just analyticalToolProposalId) Nothing Nothing Nothing Nothing Nothing (Just 1) (Just 1)
{-# INLINE  readAnalyticalToolProposalByPrimaryKey #-}

createAnalyticalToolTransaction
  :: ()
  => AnalyticalToolTransaction -- ^ DATA
  -> CurlClientM NoContent
readAnalyticalToolTransaction
  :: ()
  => Maybe AnalyticalToolTransactionId -- ^ _id
  -> Maybe AnalyticalToolId -- ^ analytic_tool_id
  -> Maybe TransactionId -- ^ transaction_id
  -> Maybe LocalTime -- ^ created
  -> Maybe LocalTime -- ^ deleted
  -> Maybe LocalTime -- ^ updated
  -> Maybe Int -- ^ items_per_page
  -> Maybe Int -- ^ page_number
  -> CurlClientM [AnalyticalToolTransaction]
updateAnalyticalToolTransaction
  :: ()
  => Maybe AnalyticalToolTransactionId -- ^ _id
  -> AnalyticalToolTransaction -- ^ DATA
  -> CurlClientM NoContent
destroyAnalyticalToolTransaction
  :: ()
  => Maybe AnalyticalToolTransactionId -- ^ _id
  -> CurlClientM NoContent
createAnalyticalToolTransaction :<|> readAnalyticalToolTransaction :<|> updateAnalyticalToolTransaction :<|> destroyAnalyticalToolTransaction = toClient apiAnalyticalToolTransaction def
{-# NOINLINE  createAnalyticalToolTransaction #-}
{-# NOINLINE  readAnalyticalToolTransaction #-}
{-# NOINLINE  updateAnalyticalToolTransaction #-}
{-# NOINLINE  destroyAnalyticalToolTransaction #-}

readAnalyticalToolTransactionByPrimaryKey
  :: ()
  => AnalyticalToolTransactionId -- ^ _id
  -> CurlClientM (Maybe AnalyticalToolTransaction)
readAnalyticalToolTransactionByPrimaryKey analyticalToolTransactionId = safeHead <$> readAnalyticalToolTransaction (Just analyticalToolTransactionId) Nothing Nothing Nothing Nothing Nothing (Just 1) (Just 1)
{-# INLINE  readAnalyticalToolTransactionByPrimaryKey #-}

createCitation
  :: ()
  => Citation -- ^ DATA
  -> CurlClientM NoContent
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
  -> CurlClientM [Citation]
updateCitation
  :: ()
  => Maybe CitationId -- ^ _id
  -> Citation -- ^ DATA
  -> CurlClientM NoContent
destroyCitation
  :: ()
  => Maybe CitationId -- ^ _id
  -> CurlClientM NoContent
createCitation :<|> readCitation :<|> updateCitation :<|> destroyCitation = toClient apiCitation def
{-# NOINLINE  createCitation #-}
{-# NOINLINE  readCitation #-}
{-# NOINLINE  updateCitation #-}
{-# NOINLINE  destroyCitation #-}

readCitationByPrimaryKey
  :: ()
  => CitationId -- ^ _id
  -> CurlClientM (Maybe Citation)
readCitationByPrimaryKey citationId = safeHead <$> readCitation (Just citationId) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing (Just 1) (Just 1)
{-# INLINE  readCitationByPrimaryKey #-}

createCitationContributor
  :: ()
  => CitationContributor -- ^ DATA
  -> CurlClientM NoContent
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
  -> CurlClientM [CitationContributor]
updateCitationContributor
  :: ()
  => Maybe CitationContributorId -- ^ _id
  -> CitationContributor -- ^ DATA
  -> CurlClientM NoContent
destroyCitationContributor
  :: ()
  => Maybe CitationContributorId -- ^ _id
  -> CurlClientM NoContent
createCitationContributor :<|> readCitationContributor :<|> updateCitationContributor :<|> destroyCitationContributor = toClient apiCitationContributor def
{-# NOINLINE  createCitationContributor #-}
{-# NOINLINE  readCitationContributor #-}
{-# NOINLINE  updateCitationContributor #-}
{-# NOINLINE  destroyCitationContributor #-}

readCitationContributorByPrimaryKey
  :: ()
  => CitationContributorId -- ^ _id
  -> CurlClientM (Maybe CitationContributor)
readCitationContributorByPrimaryKey citationContributorId = safeHead <$> readCitationContributor (Just citationContributorId) Nothing Nothing Nothing Nothing Nothing Nothing (Just 1) (Just 1)
{-# INLINE  readCitationContributorByPrimaryKey #-}

createCitationKeyword
  :: ()
  => CitationKeyword -- ^ DATA
  -> CurlClientM NoContent
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
  -> CurlClientM [CitationKeyword]
updateCitationKeyword
  :: ()
  => Maybe CitationKeywordId -- ^ _id
  -> CitationKeyword -- ^ DATA
  -> CurlClientM NoContent
destroyCitationKeyword
  :: ()
  => Maybe CitationKeywordId -- ^ _id
  -> CurlClientM NoContent
createCitationKeyword :<|> readCitationKeyword :<|> updateCitationKeyword :<|> destroyCitationKeyword = toClient apiCitationKeyword def
{-# NOINLINE  createCitationKeyword #-}
{-# NOINLINE  readCitationKeyword #-}
{-# NOINLINE  updateCitationKeyword #-}
{-# NOINLINE  destroyCitationKeyword #-}

readCitationKeywordByPrimaryKey
  :: ()
  => CitationKeywordId -- ^ _id
  -> CurlClientM (Maybe CitationKeyword)
readCitationKeywordByPrimaryKey citationKeywordId = safeHead <$> readCitationKeyword (Just citationKeywordId) Nothing Nothing Nothing Nothing Nothing (Just 1) (Just 1)
{-# INLINE  readCitationKeywordByPrimaryKey #-}

createCitationProposal
  :: ()
  => CitationProposal -- ^ DATA
  -> CurlClientM NoContent
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
  -> CurlClientM [CitationProposal]
updateCitationProposal
  :: ()
  => Maybe CitationProposalId -- ^ _id
  -> CitationProposal -- ^ DATA
  -> CurlClientM NoContent
destroyCitationProposal
  :: ()
  => Maybe CitationProposalId -- ^ _id
  -> CurlClientM NoContent
createCitationProposal :<|>readCitationProposal :<|> updateCitationProposal :<|> destroyCitationProposal = toClient apiCitationProposal def
{-# NOINLINE  createCitationProposal #-}
{-# NOINLINE  readCitationProposal #-}
{-# NOINLINE  updateCitationProposal #-}
{-# NOINLINE  destroyCitationProposal #-}

readCitationProposalByPrimaryKey
  :: ()
  => CitationProposalId -- ^ _id
  -> CurlClientM (Maybe CitationProposal)
readCitationProposalByPrimaryKey citationProposalId = safeHead <$> readCitationProposal (Just citationProposalId) Nothing Nothing Nothing Nothing Nothing (Just 1) (Just 1)
{-# INLINE  readCitationProposalByPrimaryKey #-}

createContributor
  :: ()
  => Contributor -- ^ DATA
  -> CurlClientM NoContent
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
  -> CurlClientM [Contributor]
updateContributor
  :: ()
  => Maybe ContributorId -- ^ _id
  -> Contributor -- ^ DATA
  -> CurlClientM NoContent
destroyContributor
  :: ()
  => Maybe ContributorId -- ^ _id
  -> CurlClientM NoContent
createContributor :<|> readContributor :<|> updateContributor :<|> destroyContributor = toClient apiContributor def
{-# NOINLINE  createContributor #-}
{-# NOINLINE  readContributor #-}
{-# NOINLINE  updateContributor #-}
{-# NOINLINE  destroyContributor #-}

readContributorByPrimaryKey
  :: ()
  => ContributorId -- ^ _id
  -> CurlClientM (Maybe Contributor)
readContributorByPrimaryKey contributorId = safeHead <$> readContributor (Just contributorId) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing (Just 1) (Just 1)
{-# INLINE  readContributorByPrimaryKey #-}

createFile
  :: ()
  => File -- ^ DATA
  -> CurlClientM NoContent
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
  -> CurlClientM [File]
updateFile
  :: ()
  => Maybe FileId -- ^ _id
  -> File -- ^ DATA
  -> CurlClientM NoContent
destroyFile
  :: ()
  => Maybe FileId -- ^ _id
  -> CurlClientM NoContent
createFile :<|> readFile :<|> updateFile :<|> destroyFile = toClient apiFile def
{-# NOINLINE  createFile #-}
{-# NOINLINE  readFile #-}
{-# NOINLINE  updateFile #-}
{-# NOINLINE  destroyFile #-}

readFileByPrimaryKey
  :: ()
  => FileId
  -> CurlClientM (Maybe File)
readFileByPrimaryKey fileId = safeHead <$> readFile (Just fileId) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing (Just 1) (Just 1)
{-# INLINE  readFileByPrimaryKey #-}

createFileKeyValue
  :: ()
  => FileKeyValue -- ^ DATA
  -> CurlClientM NoContent
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
  -> CurlClientM [FileKeyValue]
updateFileKeyValue
  :: ()
  => Maybe FileKeyValueId -- ^ _id
  -> FileKeyValue -- ^ DATA
  -> CurlClientM NoContent
destroyFileKeyValue
  :: ()
  => Maybe FileKeyValueId -- ^ _id
  -> CurlClientM NoContent
createFileKeyValue :<|> readFileKeyValue :<|> updateFileKeyValue :<|> destroyFileKeyValue = toClient apiFileKeyValue def
{-# NOINLINE  createFileKeyValue #-}
{-# NOINLINE  readFileKeyValue #-}
{-# NOINLINE  updateFileKeyValue #-}
{-# NOINLINE  destroyFileKeyValue #-}

readFileKeyValueByPrimaryKey
  :: ()
  => FileKeyValueId
  -> CurlClientM (Maybe FileKeyValue)
readFileKeyValueByPrimaryKey fileKeyValueId = safeHead <$> readFileKeyValue (Just fileKeyValueId) Nothing Nothing Nothing Nothing Nothing Nothing (Just 1) (Just 1)
{-# INLINE  readFileKeyValueByPrimaryKey #-}

createGroup
  :: ()
  => Group -- ^ DATA
  -> CurlClientM NoContent
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
  -> CurlClientM [Group]
updateGroup
  :: ()
  => Maybe GroupId -- ^ _id
  -> Group -- ^ DATA
  -> CurlClientM NoContent
destroyGroup
  :: ()
  => Maybe GroupId -- ^ _id
  -> CurlClientM NoContent
createGroup :<|> readGroup :<|> updateGroup :<|> destroyGroup = toClient apiGroup def
{-# NOINLINE  createGroup #-}
{-# NOINLINE  readGroup #-}
{-# NOINLINE  updateGroup #-}
{-# NOINLINE  destroyGroup #-}

readGroupByPrimaryKey
  :: ()
  => GroupId
  -> CurlClientM (Maybe Group)
readGroupByPrimaryKey groupId = safeHead <$> readGroup (Just groupId) Nothing Nothing Nothing Nothing Nothing Nothing (Just 1) (Just 1)
{-# INLINE  readGroupByPrimaryKey #-}

createInstitution
  :: ()
  => Institution -- ^ DATA
  -> CurlClientM NoContent
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
  -> CurlClientM [Institution]
updateInstitution
  :: ()
  => Maybe InstitutionId -- ^ _id
  -> Institution -- ^ DATA
  -> CurlClientM NoContent
destroyInstitution
  :: ()
  => Maybe InstitutionId -- ^ _id
  -> CurlClientM NoContent
createInstitution :<|> readInstitution :<|> updateInstitution :<|> destroyInstitution = toClient apiInstitution def
{-# NOINLINE  createInstitution #-}
{-# NOINLINE  readInstitution #-}
{-# NOINLINE  updateInstitution #-}
{-# NOINLINE  destroyInstitution #-}

readInstitutionByPrimaryKey
  :: ()
  => InstitutionId
  -> CurlClientM (Maybe Institution)
readInstitutionByPrimaryKey institutionId = safeHead <$> readInstitution (Just institutionId) Nothing Nothing Nothing Nothing Nothing Nothing Nothing (Just 1) (Just 1)
{-# INLINE readInstitutionByPrimaryKey #-}

createInstitutionPerson
  :: ()
  => InstitutionPerson -- ^ DATA
  -> CurlClientM NoContent
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
  -> CurlClientM [InstitutionPerson]
updateInstitutionPerson
  :: ()
  => Maybe InstitutionPersonId -- ^ _id
  -> InstitutionPerson -- ^ DATA
  -> CurlClientM NoContent
destroyInstitutionPerson
  :: ()
  => Maybe InstitutionPersonId -- ^ _id
  -> CurlClientM NoContent
createInstitutionPerson :<|> readInstitutionPerson :<|> updateInstitutionPerson :<|> destroyInstitutionPerson = toClient apiInstitutionPerson def
{-# NOINLINE  createInstitutionPerson #-}
{-# NOINLINE  readInstitutionPerson #-}
{-# NOINLINE  updateInstitutionPerson #-}
{-# NOINLINE  destroyInstitutionPerson #-}

readInstitutionPersonByPrimaryKey
  :: ()
  => InstitutionPersonId
  -> CurlClientM (Maybe InstitutionPerson)
readInstitutionPersonByPrimaryKey institutionPersonId = safeHead <$> readInstitutionPerson (Just institutionPersonId) Nothing Nothing Nothing Nothing Nothing (Just 1) (Just 1)
{-# INLINE  readInstitutionPersonByPrimaryKey #-}

createInstrument
  :: ()
  => Instrument -- ^ DATA
  -> CurlClientM NoContent
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
  -> CurlClientM [Instrument]
updateInstrument
  :: ()
  => Maybe InstrumentId -- ^ _id
  -> Instrument -- ^ DATA
  -> CurlClientM NoContent
destroyInstrument
  :: ()
  => Maybe InstrumentId -- ^ _id
  -> CurlClientM NoContent
createInstrument :<|> readInstrument :<|> updateInstrument :<|> destroyInstrument = toClient apiInstrument def
{-# NOINLINE  createInstrument #-}
{-# NOINLINE  readInstrument #-}
{-# NOINLINE  updateInstrument #-}
{-# NOINLINE  destroyInstrument #-}

readInstrumentByPrimaryKey
  :: ()
  => InstrumentId
  -> CurlClientM (Maybe Instrument)
readInstrumentByPrimaryKey instrumentId = safeHead <$> readInstrument (Just instrumentId) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing (Just 1) (Just 1)
{-# INLINE  readInstrumentByPrimaryKey #-}

createInstrumentCustodian
  :: ()
  => InstrumentCustodian -- ^ DATA
  -> CurlClientM NoContent
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
  -> CurlClientM [InstrumentCustodian]
updateInstrumentCustodian
  :: ()
  => Maybe InstrumentCustodianId -- ^ _id
  -> InstrumentCustodian -- ^ DATA
  -> CurlClientM NoContent
destroyInstrumentCustodian
  :: ()
  => Maybe InstrumentCustodianId -- ^ _id
  -> CurlClientM NoContent
createInstrumentCustodian :<|> readInstrumentCustodian :<|> updateInstrumentCustodian :<|> destroyInstrumentCustodian = toClient apiInstrumentCustodian def
{-# NOINLINE  createInstrumentCustodian #-}
{-# NOINLINE  readInstrumentCustodian #-}
{-# NOINLINE  updateInstrumentCustodian #-}
{-# NOINLINE  destroyInstrumentCustodian #-}

readInstrumentCustodianByPrimaryKey
  :: ()
  => InstrumentCustodianId
  -> CurlClientM (Maybe InstrumentCustodian)
readInstrumentCustodianByPrimaryKey instrumentCustodianId = safeHead <$> readInstrumentCustodian (Just instrumentCustodianId) Nothing Nothing Nothing Nothing Nothing (Just 1) (Just 1)
{-# INLINE  readInstrumentCustodianByPrimaryKey #-}

createInstrumentGroup
  :: ()
  => InstrumentGroup -- ^ DATA
  -> CurlClientM NoContent
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
  -> CurlClientM [InstrumentGroup]
updateInstrumentGroup
  :: ()
  => Maybe InstrumentGroupId -- ^ _id
  -> InstrumentGroup -- ^ DATA
  -> CurlClientM NoContent
destroyInstrumentGroup
  :: ()
  => Maybe InstrumentGroupId -- ^ _id
  -> CurlClientM NoContent
createInstrumentGroup :<|> readInstrumentGroup :<|> updateInstrumentGroup :<|> destroyInstrumentGroup = toClient apiInstrumentGroup def
{-# NOINLINE  createInstrumentGroup #-}
{-# NOINLINE  readInstrumentGroup #-}
{-# NOINLINE  updateInstrumentGroup #-}
{-# NOINLINE  destroyInstrumentGroup #-}

readInstrumentGroupByPrimaryKey
  :: ()
  => InstrumentGroupId
  -> CurlClientM (Maybe InstrumentGroup)
readInstrumentGroupByPrimaryKey instrumentGroupId = safeHead <$> readInstrumentGroup (Just instrumentGroupId) Nothing Nothing Nothing Nothing Nothing (Just 1) (Just 1)
{-# INLINE  readInstrumentGroupByPrimaryKey #-}

createJournal
  :: ()
  => Journal -- ^ DATA
  -> CurlClientM NoContent
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
  -> CurlClientM [Journal]
updateJournal
  :: ()
  => Maybe JournalId -- ^ _id
  -> Journal -- ^ DATA
  -> CurlClientM NoContent
destroyJournal
  :: ()
  => Maybe JournalId -- ^ _id
  -> CurlClientM NoContent
createJournal :<|> readJournal :<|> updateJournal :<|> destroyJournal = toClient apiJournal def
{-# NOINLINE  createJournal #-}
{-# NOINLINE  readJournal #-}
{-# NOINLINE  updateJournal #-}
{-# NOINLINE  destroyJournal #-}

readJournalByPrimaryKey
  :: ()
  => JournalId
  -> CurlClientM (Maybe Journal)
readJournalByPrimaryKey journalId = safeHead <$> readJournal (Just journalId) Nothing Nothing Nothing Nothing Nothing Nothing Nothing (Just 1) (Just 1)
{-# INLINE  readJournalByPrimaryKey #-}

createKey
  :: ()
  => Key -- ^ DATA
  -> CurlClientM NoContent
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
  -> CurlClientM [Key]
updateKey
  :: ()
  => Maybe KeyId -- ^ _id
  -> Key -- ^ DATA
  -> CurlClientM NoContent
destroyKey
  :: ()
  => Maybe KeyId -- ^ _id
  -> CurlClientM NoContent
createKey :<|> readKey :<|> updateKey :<|> destroyKey = toClient apiKey def
{-# NOINLINE  createKey #-}
{-# NOINLINE  readKey #-}
{-# NOINLINE  updateKey #-}
{-# NOINLINE  destroyKey #-}

readKeyByPrimaryKey
  :: ()
  => KeyId
  -> CurlClientM (Maybe Key)
readKeyByPrimaryKey keyId = safeHead <$> readKey (Just keyId) Nothing Nothing Nothing Nothing Nothing (Just 1) (Just 1)
{-# INLINE  readKeyByPrimaryKey #-}

createKeyword
  :: ()
  => Keyword -- ^ DATA
  -> CurlClientM NoContent
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
  -> CurlClientM [Keyword]
updateKeyword
  :: ()
  => Maybe KeywordId -- ^ _id
  -> Keyword -- ^ DATA
  -> CurlClientM NoContent
destroyKeyword
  :: ()
  => Maybe KeywordId -- ^ _id
  -> CurlClientM NoContent
createKeyword :<|> readKeyword :<|> updateKeyword :<|> destroyKeyword = toClient apiKeyword def
{-# NOINLINE  createKeyword #-}
{-# NOINLINE  readKeyword #-}
{-# NOINLINE  updateKeyword #-}
{-# NOINLINE  destroyKeyword #-}

readKeywordByPrimaryKey
  :: ()
  => KeywordId
  -> CurlClientM (Maybe Keyword)
readKeywordByPrimaryKey keywordId = safeHead <$> readKeyword (Just keywordId) Nothing Nothing Nothing Nothing Nothing (Just 1) (Just 1)
{-# INLINE  readKeywordByPrimaryKey #-}

createProposal
  :: ()
  => Proposal -- ^ DATA
  -> CurlClientM NoContent
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
  -> CurlClientM [Proposal]
updateProposal
  :: ()
  => Maybe ProposalId -- ^ _id
  -> Proposal -- ^ DATA
  -> CurlClientM NoContent
destroyProposal
  :: ()
  => Maybe ProposalId -- ^ _id
  -> CurlClientM NoContent
createProposal :<|> readProposal :<|> updateProposal :<|> destroyProposal = toClient apiProposal def
{-# NOINLINE  createProposal #-}
{-# NOINLINE  readProposal #-}
{-# NOINLINE  updateProposal #-}
{-# NOINLINE  destroyProposal #-}

readProposalByPrimaryKey
  :: ()
  => ProposalId -- ^ _id
  -> CurlClientM (Maybe Proposal)
readProposalByPrimaryKey proposalId = safeHead <$> readProposal (Just proposalId) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing (Just 1) (Just 1)
{-# INLINE  readProposalByPrimaryKey #-}

createProposalInstrument
  :: ()
  => ProposalInstrument -- ^ DATA
  -> CurlClientM NoContent
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
  -> CurlClientM [ProposalInstrument]
updateProposalInstrument
  :: ()
  => Maybe ProposalInstrumentId -- ^ _id
  -> ProposalInstrument -- ^ DATA
  -> CurlClientM NoContent
destroyProposalInstrument
  :: ()
  => Maybe ProposalInstrumentId -- ^ _id
  -> CurlClientM NoContent
createProposalInstrument :<|> readProposalInstrument :<|> updateProposalInstrument :<|> destroyProposalInstrument = toClient apiProposalInstrument def
{-# NOINLINE  createProposalInstrument #-}
{-# NOINLINE  readProposalInstrument #-}
{-# NOINLINE  updateProposalInstrument #-}
{-# NOINLINE  destroyProposalInstrument #-}

readProposalInstrumentByPrimaryKey
  :: ()
  => ProposalInstrumentId
  -> CurlClientM (Maybe ProposalInstrument)
readProposalInstrumentByPrimaryKey proposalInstrumentId = safeHead <$> readProposalInstrument (Just proposalInstrumentId) Nothing Nothing Nothing Nothing Nothing (Just 1) (Just 1)
{-# INLINE  readProposalInstrumentByPrimaryKey #-}

createProposalParticipant
  :: ()
  => ProposalParticipant -- ^ DATA
  -> CurlClientM NoContent
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
  -> CurlClientM [ProposalParticipant]
updateProposalParticipant
  :: ()
  => Maybe ProposalParticipantId -- ^ _id
  -> ProposalParticipant -- ^ DATA
  -> CurlClientM NoContent
destroyProposalParticipant
  :: ()
  => Maybe ProposalParticipantId -- ^ _id
  -> CurlClientM NoContent
createProposalParticipant :<|> readProposalParticipant :<|> updateProposalParticipant :<|> destroyProposalParticipant = toClient apiProposalParticipant def
{-# NOINLINE  createProposalParticipant #-}
{-# NOINLINE  readProposalParticipant #-}
{-# NOINLINE  updateProposalParticipant #-}
{-# NOINLINE  destroyProposalParticipant #-}

readProposalParticipantByPrimaryKey
  :: ()
  => ProposalParticipantId
  -> CurlClientM (Maybe ProposalParticipant)
readProposalParticipantByPrimaryKey proposalParticipantId = safeHead <$> readProposalParticipant (Just proposalParticipantId) Nothing Nothing Nothing Nothing Nothing (Just 1) (Just 1)
{-# INLINE  readProposalParticipantByPrimaryKey #-}

createTransaction
  :: ()
  => Transaction -- ^ DATA
  -> CurlClientM NoContent
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
  -> CurlClientM [Transaction]
updateTransaction
  :: ()
  => Maybe TransactionId -- ^ _id
  -> Transaction -- ^ DATA
  -> CurlClientM NoContent
destroyTransaction
  :: ()
  => Maybe TransactionId -- ^ _id
  -> CurlClientM NoContent
createTransaction :<|> readTransaction :<|> updateTransaction :<|> destroyTransaction = toClient apiTransaction def
{-# NOINLINE  createTransaction #-}
{-# NOINLINE  readTransaction #-}
{-# NOINLINE  updateTransaction #-}
{-# NOINLINE  destroyTransaction #-}

readTransactionByPrimaryKey
  :: ()
  => TransactionId
  -> CurlClientM (Maybe Transaction)
readTransactionByPrimaryKey transactionId = safeHead <$> readTransaction (Just transactionId) Nothing Nothing Nothing Nothing Nothing Nothing (Just 1) (Just 1)
{-# INLINE  readTransactionByPrimaryKey #-}

createTransactionKeyValue
  :: ()
  => TransactionKeyValue -- ^ DATA
  -> CurlClientM NoContent
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
  -> CurlClientM [TransactionKeyValue]
updateTransactionKeyValue
  :: ()
  => Maybe TransactionKeyValueId -- ^ _id
  -> TransactionKeyValue -- ^ DATA
  -> CurlClientM NoContent
destroyTransactionKeyValue
  :: ()
  => Maybe TransactionKeyValueId -- ^ _id
  -> CurlClientM NoContent
createTransactionKeyValue :<|> readTransactionKeyValue :<|> updateTransactionKeyValue :<|> destroyTransactionKeyValue = toClient apiTransactionKeyValue def
{-# NOINLINE  createTransactionKeyValue #-}
{-# NOINLINE  readTransactionKeyValue #-}
{-# NOINLINE  updateTransactionKeyValue #-}
{-# NOINLINE  destroyTransactionKeyValue #-}

readTransactionKeyValueByPrimaryKey
  :: ()
  => TransactionKeyValueId
  -> CurlClientM (Maybe TransactionKeyValue)
readTransactionKeyValueByPrimaryKey transactionKeyValueId = safeHead <$> readTransactionKeyValue (Just transactionKeyValueId) Nothing Nothing Nothing Nothing Nothing Nothing (Just 1) (Just 1)
{-# INLINE  readTransactionKeyValueByPrimaryKey #-}

createUser
  :: ()
  => User -- ^ DATA
  -> CurlClientM NoContent
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
  -> CurlClientM [User]
updateUser
  :: ()
  => Maybe UserId -- ^ _id
  -> User -- ^ DATA
  -> CurlClientM NoContent
destroyUser
  :: ()
  => Maybe UserId -- ^ _id
  -> CurlClientM NoContent
createUser :<|> readUser :<|> updateUser :<|> destroyUser = toClient apiUser def
{-# NOINLINE  createUser #-}
{-# NOINLINE  readUser #-}
{-# NOINLINE  updateUser #-}
{-# NOINLINE  destroyUser #-}

readUserByPrimaryKey
  :: ()
  => UserId -- ^ _id
  -> CurlClientM (Maybe User)
readUserByPrimaryKey userId = safeHead <$> readUser (Just userId) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing (Just 1) (Just 1)
{-# INLINE  readUserByPrimaryKey #-}

createUserGroup
  :: ()
  => UserGroup -- ^ DATA
  -> CurlClientM NoContent
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
  -> CurlClientM [UserGroup]
updateUserGroup
  :: ()
  => Maybe UserGroupId -- ^ _id
  -> UserGroup -- ^ DATA
  -> CurlClientM NoContent
destroyUserGroup
  :: ()
  => Maybe UserGroupId -- ^ _id
  -> CurlClientM NoContent
createUserGroup :<|> readUserGroup :<|> updateUserGroup :<|> destroyUserGroup = toClient apiUserGroup def
{-# NOINLINE  createUserGroup #-}
{-# NOINLINE  readUserGroup #-}
{-# NOINLINE  updateUserGroup #-}
{-# NOINLINE  destroyUserGroup #-}

readUserGroupByPrimaryKey
  :: ()
  => UserGroupId -- ^ _id
  -> CurlClientM (Maybe UserGroup)
readUserGroupByPrimaryKey userGroupId = safeHead <$> readUserGroup (Just userGroupId) Nothing Nothing Nothing Nothing Nothing (Just 1) (Just 1)
{-# INLINE  readUserGroupByPrimaryKey #-}

createValue
  :: ()
  => Value -- ^ DATA
  -> CurlClientM NoContent
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
  -> CurlClientM [Value]
updateValue
  :: ()
  => Maybe ValueId -- ^ _id
  -> Value -- ^ DATA
  -> CurlClientM NoContent
destroyValue
  :: ()
  => Maybe ValueId -- ^ _id
  -> CurlClientM NoContent
createValue :<|> readValue :<|> updateValue :<|> destroyValue = toClient apiValue def
{-# NOINLINE  createValue #-}
{-# NOINLINE  readValue #-}
{-# NOINLINE  updateValue #-}
{-# NOINLINE  destroyValue #-}

readValueByPrimaryKey
  :: ()
  => ValueId -- ^ _id
  -> CurlClientM (Maybe Value)
readValueByPrimaryKey valueId = safeHead <$> readValue (Just valueId) Nothing Nothing Nothing Nothing Nothing (Just 1) (Just 1)
{-# INLINE  readValueByPrimaryKey #-}

-- | A link to an endpoint that is described by a Servant API type.
--
data Link = Link
  { _linkSegments :: [String] -- ^ Path segments (in order).
  , _linkQueryParams :: [(String, String)] -- ^ Query parameters.
  , _linkRequestMethod :: Maybe StdMethod -- ^ HTTP request method.
  , _linkRequestBody :: Maybe ByteString -- ^ HTTP request body.
  } deriving (Eq, Ord, Read, Show)

-- | Empty HTTP GET request to root endpoint, i.e., no segments, no query parameters, and no request body.
--
instance Default Link where
  def = Link
    { _linkSegments = []
    , _linkQueryParams = []
    , _linkRequestMethod = Nothing
    , _linkRequestBody = Nothing
    }
  {-# INLINE  def #-}

-- | Add a query parameter.
--
addQueryParam :: String -> String -> Link -> Link
addQueryParam k v l = l { _linkQueryParams = snoc (k, v) $ _linkQueryParams l }
{-# INLINE  addQueryParam #-}

-- | Add a path segment.
--
addSegment :: String -> Link -> Link
addSegment seg l = l { _linkSegments = snoc seg $ _linkSegments l }
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

-- | Run a 'Link' with the specified decoder function.
--
runLinkWith :: Integer -> (ByteString -> Either String a) -> Link -> CurlRequest a
runLinkWith n eitherDecode l = CurlRequest
  { _curlRequestBody = _linkRequestBody l
  , _curlRequestMethod = _linkRequestMethod l
  , _curlRequestMkURL = \url_type0 -> pure (URL url_type0) <*> (Data.List.intercalate "/" . _linkSegments) <*> _linkQueryParams $ l
  , _curlRequestDecode = eitherDecode
  , _curlRequestStatusCode = n
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
  type MkClient (Verb method statusCode '[JSON] NoContent) = CurlClientM NoContent
  toClient (Proxy :: Proxy (Verb method statusCode '[JSON] NoContent)) = Network.Curl.Client.fromCurlRequest . runLinkWith statusCode (const $ Right NoContent) . setRequestMethod (Proxy :: Proxy method)
    where
      statusCode :: Integer
      statusCode = GHC.TypeLits.natVal (Proxy :: Proxy statusCode)
  {-# INLINE  toClient #-}

instance {-# OVERLAPPABLE #-} (ReflectMethod method, KnownNat statusCode, FromJSON a) => HasClient (Verb (method :: StdMethod) (statusCode :: Nat) '[JSON] a) where
  type MkClient (Verb method statusCode '[JSON] a) = CurlClientM a
  toClient (Proxy :: Proxy (Verb method statusCode '[JSON] a)) = Network.Curl.Client.fromCurlRequest . runLinkWith statusCode Data.Aeson.eitherDecode' . setRequestMethod (Proxy :: Proxy method)
    where
      statusCode :: Integer
      statusCode = GHC.TypeLits.natVal (Proxy :: Proxy statusCode)
  {-# INLINE  toClient #-}

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
