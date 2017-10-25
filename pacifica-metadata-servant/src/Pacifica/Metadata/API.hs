{-# LANGUAGE  DataKinds #-}
{-# LANGUAGE  TypeOperators #-}

-- |
-- Module:      Pacifica.Metadata.API
-- Copyright:   (c) 2017 Pacific Northwest National Laboratory
-- License:     LGPL
-- Maintainer:  Mark Borkum <mark.borkum@pnnl.gov>
-- Stability:   experimental
-- Portability: portable
--
-- This module exports the Servant API type for Pacifica Metadata Services.
--
module Pacifica.Metadata.API where

import           Data.Proxy (Proxy(..))
import           Data.Text (Text)
import           Data.Time.Calendar (Day)
import           Data.Time.LocalTime (LocalTime)
import           Pacifica.Metadata.Types
import           Servant.API.Alternative ((:<|>))
import           Servant.API.ContentTypes (JSON, NoContent)
import           Servant.API.QueryParam (QueryParam)
import           Servant.API.ReqBody (ReqBody)
import           Servant.API.Sub ((:>))
import           Servant.API.Verbs (DeleteNoContent, Get, PostNoContent, PutNoContent)

type API
  =    AnalyticalToolAPI
  :<|> AnalyticalToolProposalAPI
  :<|> AnalyticalToolTransactionAPI
  :<|> CitationAPI
  :<|> CitationContributorAPI
  :<|> CitationKeywordAPI
  :<|> CitationProposalAPI
  :<|> ContributorAPI
  :<|> FileAPI
  :<|> FileKeyValueAPI
  :<|> GroupAPI
  :<|> InstitutionAPI
  :<|> InstitutionPersonAPI
  :<|> InstrumentAPI
  :<|> InstrumentCustodianAPI
  :<|> InstrumentGroupAPI
  :<|> JournalAPI
  :<|> KeyAPI
  :<|> KeywordAPI
  :<|> ProposalAPI
  :<|> ProposalInstrumentAPI
  :<|> ProposalParticipantAPI
  :<|> TransactionAPI
  :<|> TransactionKeyValueAPI
  :<|> UserAPI
  :<|> UserGroupAPI
  :<|> ValueAPI

type AnalyticalToolAPI
  = "analytical_tools"
    :> ReqBody '[JSON] AnalyticalTool
    :> PutNoContent '[JSON] NoContent
  :<|> "analytical_tools"
    :> QueryParam "_id" AnalyticalToolId
    :> QueryParam "encoding" Text
    :> QueryParam "name" Text
    :> QueryParam "created" LocalTime
    :> QueryParam "deleted" LocalTime
    :> QueryParam "updated" LocalTime
    :> QueryParam "items_per_page" Int
    :> QueryParam "page_number" Int
    :> Get '[JSON] [AnalyticalTool]
  :<|> "analytical_tools"
    :> QueryParam "_id" AnalyticalToolId
    :> ReqBody '[JSON] AnalyticalTool
    :> PostNoContent '[JSON] NoContent
  :<|> "analytical_tools"
    :> QueryParam "_id" AnalyticalToolId
    :> DeleteNoContent '[JSON] NoContent

type AnalyticalToolProposalAPI
  = "atool_proposal"
    :> ReqBody '[JSON] AnalyticalToolProposal
    :> PutNoContent '[JSON] NoContent
  :<|> "atool_proposal"
    :> QueryParam "_id" AnalyticalToolProposalId
    :> QueryParam "analytical_tool" AnalyticalToolId
    :> QueryParam "proposal" ProposalId
    :> QueryParam "created" LocalTime
    :> QueryParam "deleted" LocalTime
    :> QueryParam "updated" LocalTime
    :> QueryParam "items_per_page" Int
    :> QueryParam "page_number" Int
    :> Get '[JSON] [AnalyticalToolProposal]
  :<|> "atool_proposal"
    :> QueryParam "_id" AnalyticalToolProposalId
    :> ReqBody '[JSON] AnalyticalToolProposal
    :> PostNoContent '[JSON] NoContent
  :<|> "atool_proposal"
    :> QueryParam "_id" AnalyticalToolProposalId
    :> DeleteNoContent '[JSON] NoContent

type AnalyticalToolTransactionAPI
  = "atool_transaction"
    :> ReqBody '[JSON] AnalyticalToolTransaction
    :> PutNoContent '[JSON] NoContent
  :<|> "atool_transaction"
    :> QueryParam "_id" AnalyticalToolTransactionId
    :> QueryParam "analytical_tool" AnalyticalToolId
    :> QueryParam "transaction" TransactionId
    :> QueryParam "created" LocalTime
    :> QueryParam "deleted" LocalTime
    :> QueryParam "updated" LocalTime
    :> QueryParam "items_per_page" Int
    :> QueryParam "page_number" Int
    :> Get '[JSON] [AnalyticalToolTransaction]
  :<|> "atool_transaction"
    :> QueryParam "_id" AnalyticalToolTransactionId
    :> ReqBody '[JSON] AnalyticalToolTransaction
    :> PostNoContent '[JSON] NoContent
  :<|> "atool_transaction"
    :> QueryParam "_id" AnalyticalToolTransactionId
    :> DeleteNoContent '[JSON] NoContent

type CitationAPI
  = "citations"
    :> ReqBody '[JSON] Citation
    :> PutNoContent '[JSON] NoContent
  :<|> "citations"
    :> QueryParam "_id" CitationId
    :> QueryParam "abstract_text" Text
    :> QueryParam "article_title" Text
    :> QueryParam "doi_reference" Text
    :> QueryParam "encoding" Text
    :> QueryParam "journal" JournalId
    :> QueryParam "journal_issue" Int
    :> QueryParam "journal_volume" Int
    :> QueryParam "page_range" Text
    :> QueryParam "release_authorization_id" ReleaseAuthorizationId
    :> QueryParam "xml_text" Text
    :> QueryParam "created" LocalTime
    :> QueryParam "deleted" LocalTime
    :> QueryParam "updated" LocalTime
    :> QueryParam "items_per_page" Int
    :> QueryParam "page_number" Int
    :> Get '[JSON] [Citation]
  :<|> "citations"
    :> QueryParam "_id" CitationId
    :> ReqBody '[JSON] Citation
    :> PostNoContent '[JSON] NoContent
  :<|> "citations"
    :> QueryParam "_id" CitationId
    :> DeleteNoContent '[JSON] NoContent

type CitationContributorAPI
  = "citation_contributor"
    :> ReqBody '[JSON] CitationContributor
    :> PutNoContent '[JSON] NoContent
  :<|> "citation_contributor"
    :> QueryParam "_id" CitationContributorId
    :> QueryParam "author" ContributorId
    :> QueryParam "author_precedence" Int
    :> QueryParam "citation" CitationId
    :> QueryParam "created" LocalTime
    :> QueryParam "deleted" LocalTime
    :> QueryParam "updated" LocalTime
    :> QueryParam "items_per_page" Int
    :> QueryParam "page_number" Int
    :> Get '[JSON] [CitationContributor]
  :<|> "citation_contributor"
    :> QueryParam "_id" CitationContributorId
    :> ReqBody '[JSON] CitationContributor
    :> PostNoContent '[JSON] NoContent
  :<|> "citation_contributor"
    :> QueryParam "_id" CitationContributorId
    :> DeleteNoContent '[JSON] NoContent

type CitationKeywordAPI
  = "citation_keyword"
    :> ReqBody '[JSON] CitationKeyword
    :> PutNoContent '[JSON] NoContent
  :<|> "citation_keyword"
    :> QueryParam "_id" CitationKeywordId
    :> QueryParam "citation" CitationId
    :> QueryParam "keyword" KeywordId
    :> QueryParam "created" LocalTime
    :> QueryParam "deleted" LocalTime
    :> QueryParam "updated" LocalTime
    :> QueryParam "items_per_page" Int
    :> QueryParam "page_number" Int
    :> Get '[JSON] [CitationKeyword]
  :<|> "citation_keyword"
    :> QueryParam "_id" CitationKeywordId
    :> ReqBody '[JSON] CitationKeyword
    :> PostNoContent '[JSON] NoContent
  :<|> "citation_keyword"
    :> QueryParam "_id" CitationKeywordId
    :> DeleteNoContent '[JSON] NoContent

type CitationProposalAPI
  = "citation_proposal"
    :> ReqBody '[JSON] CitationProposal
    :> PutNoContent '[JSON] NoContent
  :<|> "citation_proposal"
    :> QueryParam "_id" CitationProposalId
    :> QueryParam "citation" CitationId
    :> QueryParam "proposal" ProposalId
    :> QueryParam "created" LocalTime
    :> QueryParam "deleted" LocalTime
    :> QueryParam "updated" LocalTime
    :> QueryParam "items_per_page" Int
    :> QueryParam "page_number" Int
    :> Get '[JSON] [CitationProposal]
  :<|> "citation_proposal"
    :> QueryParam "_id" CitationProposalId
    :> ReqBody '[JSON] CitationProposal
    :> PostNoContent '[JSON] NoContent
  :<|> "citation_proposal"
    :> QueryParam "_id" CitationProposalId
    :> DeleteNoContent '[JSON] NoContent

type ContributorAPI
  = "contributors"
    :> ReqBody '[JSON] Contributor
    :> PutNoContent '[JSON] NoContent
  :<|> "contributors"
    :> QueryParam "_id" ContributorId
    :> QueryParam "dept_code" Text
    :> QueryParam "encoding" Text
    :> QueryParam "first_name" Text
    :> QueryParam "institution" InstitutionId
    :> QueryParam "last_name" Text
    :> QueryParam "middle_initial" Text
    :> QueryParam "person" UserId
    :> QueryParam "created" LocalTime
    :> QueryParam "deleted" LocalTime
    :> QueryParam "updated" LocalTime
    :> QueryParam "items_per_page" Int
    :> QueryParam "page_number" Int
    :> Get '[JSON] [Contributor]
  :<|> "contributors"
    :> QueryParam "_id" ContributorId
    :> ReqBody '[JSON] Contributor
    :> PostNoContent '[JSON] NoContent
  :<|> "contributors"
    :> QueryParam "_id" ContributorId
    :> DeleteNoContent '[JSON] NoContent

type FileAPI
  = "files"
    :> ReqBody '[JSON] File
    :> PutNoContent '[JSON] NoContent
  :<|> "files"
    :> QueryParam "_id" FileId
    :> QueryParam "ctime" LocalTime
    :> QueryParam "encoding" Text
    :> QueryParam "hashsum" Text
    :> QueryParam "hashtype" Text
    :> QueryParam "mimetype" Text
    :> QueryParam "mtime" LocalTime
    :> QueryParam "name" Text
    :> QueryParam "size" Int
    :> QueryParam "subdir" Text
    :> QueryParam "transaction" TransactionId
    :> QueryParam "created" LocalTime
    :> QueryParam "deleted" LocalTime
    :> QueryParam "updated" LocalTime
    :> QueryParam "items_per_page" Int
    :> QueryParam "page_number" Int
    :> Get '[JSON] [File]
  :<|> "files"
    :> QueryParam "_id" FileId
    :> ReqBody '[JSON] File
    :> PostNoContent '[JSON] NoContent
  :<|> "files"
    :> QueryParam "_id" FileId
    :> DeleteNoContent '[JSON] NoContent

type FileKeyValueAPI
  = "file_key_value"
    :> ReqBody '[JSON] FileKeyValue
    :> PutNoContent '[JSON] NoContent
  :<|> "file_key_value"
    :> QueryParam "_id" FileKeyValueId
    :> QueryParam "file" FileId
    :> QueryParam "key" KeyId
    :> QueryParam "value" ValueId
    :> QueryParam "created" LocalTime
    :> QueryParam "deleted" LocalTime
    :> QueryParam "updated" LocalTime
    :> QueryParam "items_per_page" Int
    :> QueryParam "page_number" Int
    :> Get '[JSON] [FileKeyValue]
  :<|> "file_key_value"
    :> QueryParam "_id" FileKeyValueId
    :> ReqBody '[JSON] FileKeyValue
    :> PostNoContent '[JSON] NoContent
  :<|> "file_key_value"
    :> QueryParam "_id" FileKeyValueId
    :> DeleteNoContent '[JSON] NoContent

type GroupAPI
  = "groups"
    :> ReqBody '[JSON] Group
    :> PutNoContent '[JSON] NoContent
  :<|> "groups"
    :> QueryParam "_id" GroupId
    :> QueryParam "encoding" Text
    :> QueryParam "is_admin" Bool
    :> QueryParam "name" Text
    :> QueryParam "created" LocalTime
    :> QueryParam "deleted" LocalTime
    :> QueryParam "updated" LocalTime
    :> QueryParam "items_per_page" Int
    :> QueryParam "page_number" Int
    :> Get '[JSON] [Group]
  :<|> "groups"
    :> QueryParam "_id" GroupId
    :> ReqBody '[JSON] Group
    :> PostNoContent '[JSON] NoContent
  :<|> "groups"
    :> QueryParam "_id" GroupId
    :> DeleteNoContent '[JSON] NoContent

type InstitutionAPI
  = "institutions"
    :> ReqBody '[JSON] Institution
    :> PutNoContent '[JSON] NoContent
  :<|> "institutions"
    :> QueryParam "_id" JournalId
    :> QueryParam "association_cd" Text
    :> QueryParam "encoding" Text
    :> QueryParam "is_foreign" Bool
    :> QueryParam "name" Text
    :> QueryParam "created" LocalTime
    :> QueryParam "deleted" LocalTime
    :> QueryParam "updated" LocalTime
    :> QueryParam "items_per_page" Int
    :> QueryParam "page_number" Int
    :> Get '[JSON] [Institution]
  :<|> "institutions"
    :> QueryParam "_id" JournalId
    :> ReqBody '[JSON] Institution
    :> PostNoContent '[JSON] NoContent
  :<|> "institutions"
    :> QueryParam "_id" JournalId
    :> DeleteNoContent '[JSON] NoContent

type InstitutionPersonAPI
  = "institution_person"
    :> ReqBody '[JSON] InstitutionPerson
    :> PutNoContent '[JSON] NoContent
  :<|> "institution_person"
    :> QueryParam "_id" InstitutionPersonId
    :> QueryParam "institution" InstitutionId
    :> QueryParam "person" UserId
    :> QueryParam "created" LocalTime
    :> QueryParam "deleted" LocalTime
    :> QueryParam "updated" LocalTime
    :> QueryParam "items_per_page" Int
    :> QueryParam "page_number" Int
    :> Get '[JSON] [InstitutionPerson]
  :<|> "institution_person"
    :> QueryParam "_id" InstitutionPersonId
    :> ReqBody '[JSON] InstitutionPerson
    :> PostNoContent '[JSON] NoContent
  :<|> "institution_person"
    :> QueryParam "_id" InstitutionPersonId
    :> DeleteNoContent '[JSON] NoContent

type InstrumentAPI
  = "instruments"
    :> ReqBody '[JSON] Instrument
    :> PutNoContent '[JSON] NoContent
  :<|> "instruments"
    :> QueryParam "_id" InstrumentId
    :> QueryParam "active" Bool
    :> QueryParam "display_name" Text
    :> QueryParam "encoding" Text
    :> QueryParam "name" Text
    :> QueryParam "name_short" Text
    :> QueryParam "created" LocalTime
    :> QueryParam "deleted" LocalTime
    :> QueryParam "updated" LocalTime
    :> QueryParam "items_per_page" Int
    :> QueryParam "page_number" Int
    :> Get '[JSON] [Instrument]
  :<|> "instruments"
    :> QueryParam "_id" InstrumentId
    :> ReqBody '[JSON] Instrument
    :> PostNoContent '[JSON] NoContent
  :<|> "instruments"
    :> QueryParam "_id" InstrumentId
    :> DeleteNoContent '[JSON] NoContent

type InstrumentCustodianAPI
  = "instrument_custodian"
    :> ReqBody '[JSON] InstrumentCustodian
    :> PutNoContent '[JSON] NoContent
  :<|> "instrument_custodian"
    :> QueryParam "_id" InstrumentCustodianId
    :> QueryParam "custodian" UserId
    :> QueryParam "instrument" InstrumentId
    :> QueryParam "created" LocalTime
    :> QueryParam "deleted" LocalTime
    :> QueryParam "updated" LocalTime
    :> QueryParam "items_per_page" Int
    :> QueryParam "page_number" Int
    :> Get '[JSON] [InstrumentCustodian]
  :<|> "instrument_custodian"
    :> QueryParam "_id" InstrumentCustodianId
    :> ReqBody '[JSON] InstrumentCustodian
    :> PostNoContent '[JSON] NoContent
  :<|> "instrument_custodian"
    :> QueryParam "_id" InstrumentCustodianId
    :> DeleteNoContent '[JSON] NoContent

type InstrumentGroupAPI
  = "instrument_group"
    :> ReqBody '[JSON] InstrumentGroup
    :> PutNoContent '[JSON] NoContent
  :<|> "instrument_group"
    :> QueryParam "_id" InstrumentGroupId
    :> QueryParam "group" GroupId
    :> QueryParam "instrument" InstrumentId
    :> QueryParam "created" LocalTime
    :> QueryParam "deleted" LocalTime
    :> QueryParam "updated" LocalTime
    :> QueryParam "items_per_page" Int
    :> QueryParam "page_number" Int
    :> Get '[JSON] [InstrumentGroup]
  :<|> "instrument_group"
    :> QueryParam "_id" InstrumentGroupId
    :> ReqBody '[JSON] InstrumentGroup
    :> PostNoContent '[JSON] NoContent
  :<|> "instrument_group"
    :> QueryParam "_id" InstrumentGroupId
    :> DeleteNoContent '[JSON] NoContent

type JournalAPI
  = "journals"
    :> ReqBody '[JSON] Journal
    :> PutNoContent '[JSON] NoContent
  :<|> "journals"
    :> QueryParam "_id" JournalId
    :> QueryParam "encoding" Text
    :> QueryParam "impact_factor" Double
    :> QueryParam "name" Text
    :> QueryParam "website_url" Text
    :> QueryParam "created" LocalTime
    :> QueryParam "deleted" LocalTime
    :> QueryParam "updated" LocalTime
    :> QueryParam "items_per_page" Int
    :> QueryParam "page_number" Int
    :> Get '[JSON] [Journal]
  :<|> "journals"
    :> QueryParam "_id" JournalId
    :> ReqBody '[JSON] Journal
    :> PostNoContent '[JSON] NoContent
  :<|> "journals"
    :> QueryParam "_id" JournalId
    :> DeleteNoContent '[JSON] NoContent

type KeyAPI
  = "keys"
    :> ReqBody '[JSON] Key
    :> PutNoContent '[JSON] NoContent
  :<|> "keys"
    :> QueryParam "_id" KeyId
    :> QueryParam "encoding" Text
    :> QueryParam "key" Text
    :> QueryParam "created" LocalTime
    :> QueryParam "deleted" LocalTime
    :> QueryParam "updated" LocalTime
    :> QueryParam "items_per_page" Int
    :> QueryParam "page_number" Int
    :> Get '[JSON] [Key]
  :<|> "keys"
    :> QueryParam "_id" KeyId
    :> ReqBody '[JSON] Key
    :> PostNoContent '[JSON] NoContent
  :<|> "keys"
    :> QueryParam "_id" KeyId
    :> DeleteNoContent '[JSON] NoContent

type KeywordAPI
  = "keywords"
    :> ReqBody '[JSON] Keyword
    :> PutNoContent '[JSON] NoContent
  :<|> "keywords"
    :> QueryParam "_id" KeywordId
    :> QueryParam "encoding" Text
    :> QueryParam "keyword" Text
    :> QueryParam "created" LocalTime
    :> QueryParam "deleted" LocalTime
    :> QueryParam "updated" LocalTime
    :> QueryParam "items_per_page" Int
    :> QueryParam "page_number" Int
    :> Get '[JSON] [Keyword]
  :<|> "keywords"
    :> QueryParam "_id" KeywordId
    :> ReqBody '[JSON] Keyword
    :> PostNoContent '[JSON] NoContent
  :<|> "keywords"
    :> QueryParam "_id" KeywordId
    :> DeleteNoContent '[JSON] NoContent

type ProposalAPI
  = "proposals"
    :> ReqBody '[JSON] Proposal
    :> PutNoContent '[JSON] NoContent
  :<|> "proposals"
    :> QueryParam "_id" ProposalId
    :> QueryParam "abstract" Text
    :> QueryParam "accepted_date" Day
    :> QueryParam "actual_end_date" Day
    :> QueryParam "actual_start_date" Day
    :> QueryParam "closed_date" Day
    :> QueryParam "encoding" Text
    :> QueryParam "proposal_type" Text
    :> QueryParam "science_theme" Text
    :> QueryParam "submitted_date" LocalTime
    :> QueryParam "title" Text
    :> QueryParam "created" LocalTime
    :> QueryParam "deleted" LocalTime
    :> QueryParam "updated" LocalTime
    :> QueryParam "items_per_page" Int
    :> QueryParam "page_number" Int
    :> Get '[JSON] [Proposal]
  :<|> "proposals"
    :> QueryParam "_id" ProposalId
    :> ReqBody '[JSON] Proposal
    :> PostNoContent '[JSON] NoContent
  :<|> "proposals"
    :> QueryParam "_id" ProposalId
    :> DeleteNoContent '[JSON] NoContent

type ProposalInstrumentAPI
  = "proposal_instrument"
    :> ReqBody '[JSON] ProposalInstrument
    :> PutNoContent '[JSON] NoContent
  :<|> "proposal_instrument"
    :> QueryParam "_id" ProposalInstrumentId
    :> QueryParam "instrument" InstrumentId
    :> QueryParam "proposal" ProposalId
    :> QueryParam "created" LocalTime
    :> QueryParam "deleted" LocalTime
    :> QueryParam "updated" LocalTime
    :> QueryParam "items_per_page" Int
    :> QueryParam "page_number" Int
    :> Get '[JSON] [ProposalInstrument]
  :<|> "proposal_instrument"
    :> QueryParam "_id" ProposalInstrumentId
    :> ReqBody '[JSON] ProposalInstrument
    :> PostNoContent '[JSON] NoContent
  :<|> "proposal_instrument"
    :> QueryParam "_id" ProposalInstrumentId
    :> DeleteNoContent '[JSON] NoContent

type ProposalParticipantAPI
  = "proposal_participant"
    :> ReqBody '[JSON] ProposalParticipant
    :> PutNoContent '[JSON] NoContent
  :<|> "proposal_participant"
    :> QueryParam "_id" ProposalParticipantId
    :> QueryParam "person" UserId
    :> QueryParam "proposal" ProposalId
    :> QueryParam "created" LocalTime
    :> QueryParam "deleted" LocalTime
    :> QueryParam "updated" LocalTime
    :> QueryParam "items_per_page" Int
    :> QueryParam "page_number" Int
    :> Get '[JSON] [ProposalParticipant]
  :<|> "proposal_participant"
    :> QueryParam "_id" ProposalParticipantId
    :> ReqBody '[JSON] ProposalParticipant
    :> PostNoContent '[JSON] NoContent
  :<|> "proposal_participant"
    :> QueryParam "_id" ProposalParticipantId
    :> DeleteNoContent '[JSON] NoContent

type TransactionAPI
  = "transactions"
    :> ReqBody '[JSON] Transaction
    :> PutNoContent '[JSON] NoContent
  :<|> "transactions"
    :> QueryParam "_id" TransactionId
    :> QueryParam "instrument" InstrumentId
    :> QueryParam "proposal" ProposalId
    :> QueryParam "submitter" UserId
    :> QueryParam "created" LocalTime
    :> QueryParam "deleted" LocalTime
    :> QueryParam "updated" LocalTime
    :> QueryParam "items_per_page" Int
    :> QueryParam "page_number" Int
    :> Get '[JSON] [Transaction]
  :<|> "transactions"
    :> QueryParam "_id" TransactionId
    :> ReqBody '[JSON] Transaction
    :> PostNoContent '[JSON] NoContent
  :<|> "transactions"
    :> QueryParam "_id" TransactionId
    :> DeleteNoContent '[JSON] NoContent

type TransactionKeyValueAPI
  = "trans_key_value"
    :> ReqBody '[JSON] TransactionKeyValue
    :> PutNoContent '[JSON] NoContent
  :<|> "trans_key_value"
    :> QueryParam "_id" TransactionKeyValueId
    :> QueryParam "key" KeyId
    :> QueryParam "transaction" TransactionId
    :> QueryParam "value" ValueId
    :> QueryParam "created" LocalTime
    :> QueryParam "deleted" LocalTime
    :> QueryParam "updated" LocalTime
    :> QueryParam "items_per_page" Int
    :> QueryParam "page_number" Int
    :> Get '[JSON] [TransactionKeyValue]
  :<|> "trans_key_value"
    :> QueryParam "_id" TransactionKeyValueId
    :> ReqBody '[JSON] TransactionKeyValue
    :> PostNoContent '[JSON] NoContent
  :<|> "trans_key_value"
    :> QueryParam "_id" TransactionKeyValueId
    :> DeleteNoContent '[JSON] NoContent

type UserAPI
  = "users"
    :> ReqBody '[JSON] User
    :> PutNoContent '[JSON] NoContent
  :<|> "users"
    :> QueryParam "_id" UserId
    :> QueryParam "encoding" Text
    :> QueryParam "first_name" Text
    :> QueryParam "last_name" Text
    :> QueryParam "middle_initial" Text
    :> QueryParam "network_id" NetworkId
    :> QueryParam "created" LocalTime
    :> QueryParam "deleted" LocalTime
    :> QueryParam "updated" LocalTime
    :> QueryParam "items_per_page" Int
    :> QueryParam "page_number" Int
    :> Get '[JSON] [User]
  :<|> "users"
    :> QueryParam "_id" UserId
    :> ReqBody '[JSON] User
    :> PostNoContent '[JSON] NoContent
  :<|> "users"
    :> QueryParam "_id" UserId
    :> DeleteNoContent '[JSON] NoContent

type UserGroupAPI
  = "user_group"
    :> ReqBody '[JSON] UserGroup
    :> PutNoContent '[JSON] NoContent
  :<|> "user_group"
    :> QueryParam "_id" UserGroupId
    :> QueryParam "group" GroupId
    :> QueryParam "person" UserId
    :> QueryParam "created" LocalTime
    :> QueryParam "deleted" LocalTime
    :> QueryParam "updated" LocalTime
    :> QueryParam "items_per_page" Int
    :> QueryParam "page_number" Int
    :> Get '[JSON] [UserGroup]
  :<|> "user_group"
    :> QueryParam "_id" UserGroupId
    :> ReqBody '[JSON] UserGroup
    :> PostNoContent '[JSON] NoContent
  :<|> "user_group"
    :> QueryParam "_id" UserGroupId
    :> DeleteNoContent '[JSON] NoContent

type ValueAPI
  = "values"
    :> ReqBody '[JSON] Value
    :> PutNoContent '[JSON] NoContent
  :<|> "values"
    :> QueryParam "_id" ValueId
    :> QueryParam "encoding" Text
    :> QueryParam "value" Text
    :> QueryParam "created" LocalTime
    :> QueryParam "deleted" LocalTime
    :> QueryParam "updated" LocalTime
    :> QueryParam "items_per_page" Int
    :> QueryParam "page_number" Int
    :> Get '[JSON] [Value]
  :<|> "values"
    :> QueryParam "_id" ValueId
    :> ReqBody '[JSON] Value
    :> PostNoContent '[JSON] NoContent
  :<|> "values"
    :> QueryParam "_id" ValueId
    :> DeleteNoContent '[JSON] NoContent

api :: Proxy API
api = Proxy
{-# INLINE  api #-}

apiAnalyticalTool :: Proxy AnalyticalToolAPI
apiAnalyticalTool = Proxy
{-# INLINE  apiAnalyticalTool #-}

apiAnalyticalToolProposal :: Proxy AnalyticalToolProposalAPI
apiAnalyticalToolProposal = Proxy
{-# INLINE  apiAnalyticalToolProposal #-}

apiAnalyticalToolTransaction :: Proxy AnalyticalToolTransactionAPI
apiAnalyticalToolTransaction = Proxy
{-# INLINE  apiAnalyticalToolTransaction #-}

apiCitation :: Proxy CitationAPI
apiCitation = Proxy
{-# INLINE  apiCitation #-}

apiCitationContributor :: Proxy CitationContributorAPI
apiCitationContributor = Proxy
{-# INLINE  apiCitationContributor #-}

apiCitationKeyword :: Proxy CitationKeywordAPI
apiCitationKeyword = Proxy
{-# INLINE  apiCitationKeyword #-}

apiCitationProposal :: Proxy CitationProposalAPI
apiCitationProposal = Proxy
{-# INLINE  apiCitationProposal #-}

apiContributor :: Proxy ContributorAPI
apiContributor = Proxy
{-# INLINE  apiContributor #-}

apiFile :: Proxy FileAPI
apiFile = Proxy
{-# INLINE  apiFile #-}

apiFileKeyValue :: Proxy FileKeyValueAPI
apiFileKeyValue = Proxy
{-# INLINE  apiFileKeyValue #-}

apiGroup :: Proxy GroupAPI
apiGroup = Proxy
{-# INLINE  apiGroup #-}

apiInstitution :: Proxy InstitutionAPI
apiInstitution = Proxy
{-# INLINE  apiInstitution #-}

apiInstitutionPerson :: Proxy InstitutionPersonAPI
apiInstitutionPerson = Proxy
{-# INLINE  apiInstitutionPerson #-}

apiInstrument :: Proxy InstrumentAPI
apiInstrument = Proxy
{-# INLINE  apiInstrument #-}

apiInstrumentCustodian :: Proxy InstrumentCustodianAPI
apiInstrumentCustodian = Proxy
{-# INLINE  apiInstrumentCustodian #-}

apiInstrumentGroup :: Proxy InstrumentGroupAPI
apiInstrumentGroup = Proxy
{-# INLINE apiInstrumentGroup #-}

apiJournal :: Proxy JournalAPI
apiJournal = Proxy
{-# INLINE  apiJournal #-}

apiKey :: Proxy KeyAPI
apiKey = Proxy
{-# INLINE  apiKey #-}

apiKeyword :: Proxy KeywordAPI
apiKeyword = Proxy
{-# INLINE  apiKeyword #-}

apiProposal :: Proxy ProposalAPI
apiProposal = Proxy
{-# INLINE  apiProposal #-}

apiProposalInstrument :: Proxy ProposalInstrumentAPI
apiProposalInstrument = Proxy
{-# INLINE  apiProposalInstrument #-}

apiProposalParticipant :: Proxy ProposalParticipantAPI
apiProposalParticipant = Proxy
{-# INLINE  apiProposalParticipant #-}

apiTransaction :: Proxy TransactionAPI
apiTransaction = Proxy
{-# INLINE  apiTransaction #-}

apiTransactionKeyValue :: Proxy TransactionKeyValueAPI
apiTransactionKeyValue = Proxy
{-# INLINE  apiTransactionKeyValue #-}

apiUser :: Proxy UserAPI
apiUser = Proxy
{-# INLINE  apiUser #-}

apiUserGroup :: Proxy UserGroupAPI
apiUserGroup = Proxy
{-# INLINE  apiUserGroup #-}

apiValue :: Proxy ValueAPI
apiValue = Proxy
{-# INLINE  apiValue #-}
