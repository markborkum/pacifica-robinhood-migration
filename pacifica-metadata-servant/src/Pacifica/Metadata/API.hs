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
    :> QueryParam "encoding" Text
    :> QueryParam "name" Text
    :> QueryParam "created" LocalTime
    :> QueryParam "deleted" LocalTime
    :> QueryParam "updated" LocalTime
    :> ReqBody '[JSON] AnalyticalTool
    :> PostNoContent '[JSON] NoContent
  :<|> "analytical_tools"
    :> QueryParam "_id" AnalyticalToolId
    :> QueryParam "encoding" Text
    :> QueryParam "name" Text
    :> QueryParam "created" LocalTime
    :> QueryParam "deleted" LocalTime
    :> QueryParam "updated" LocalTime
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
    :> QueryParam "analytical_tool" AnalyticalToolId
    :> QueryParam "proposal" ProposalId
    :> QueryParam "created" LocalTime
    :> QueryParam "deleted" LocalTime
    :> QueryParam "updated" LocalTime
    :> ReqBody '[JSON] AnalyticalToolProposal
    :> PostNoContent '[JSON] NoContent
  :<|> "atool_proposal"
    :> QueryParam "_id" AnalyticalToolProposalId
    :> QueryParam "analytical_tool" AnalyticalToolId
    :> QueryParam "proposal" ProposalId
    :> QueryParam "created" LocalTime
    :> QueryParam "deleted" LocalTime
    :> QueryParam "updated" LocalTime
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
    :> QueryParam "analytical_tool" AnalyticalToolId
    :> QueryParam "transaction" TransactionId
    :> QueryParam "created" LocalTime
    :> QueryParam "deleted" LocalTime
    :> QueryParam "updated" LocalTime
    :> ReqBody '[JSON] AnalyticalToolTransaction
    :> PostNoContent '[JSON] NoContent
  :<|> "atool_transaction"
    :> QueryParam "_id" AnalyticalToolTransactionId
    :> QueryParam "analytical_tool" AnalyticalToolId
    :> QueryParam "transaction" TransactionId
    :> QueryParam "created" LocalTime
    :> QueryParam "deleted" LocalTime
    :> QueryParam "updated" LocalTime
    :> DeleteNoContent '[JSON] NoContent

type CitationAPI
  = "citations"
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
    :> Get '[JSON] [Citation]

type CitationContributorAPI
  = "citation_contributor"
    :> QueryParam "_id" CitationContributorId
    :> QueryParam "author" ContributorId
    :> QueryParam "author_precedence" Int
    :> QueryParam "citation" CitationId
    :> QueryParam "created" LocalTime
    :> QueryParam "deleted" LocalTime
    :> QueryParam "updated" LocalTime
    :> Get '[JSON] [CitationContributor]

type CitationKeywordAPI
  = "citation_keyword"
    :> QueryParam "_id" CitationKeywordId
    :> QueryParam "citation" CitationId
    :> QueryParam "keyword" KeywordId
    :> QueryParam "created" LocalTime
    :> QueryParam "deleted" LocalTime
    :> QueryParam "updated" LocalTime
    :> Get '[JSON] [CitationKeyword]

type CitationProposalAPI
  = "citation_proposal"
    :> QueryParam "_id" CitationProposalId
    :> QueryParam "citation" CitationId
    :> QueryParam "proposal" ProposalId
    :> QueryParam "created" LocalTime
    :> QueryParam "deleted" LocalTime
    :> QueryParam "updated" LocalTime
    :> Get '[JSON] [CitationProposal]

type ContributorAPI
  = "contributors"
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
    :> Get '[JSON] [Contributor]

type FileAPI
  = "files"
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
    :> Get '[JSON] [File]

type FileKeyValueAPI
  = "file_key_value"
    :> QueryParam "_id" FileKeyValueId
    :> QueryParam "file" FileId
    :> QueryParam "key" KeyId
    :> QueryParam "value" ValueId
    :> QueryParam "created" LocalTime
    :> QueryParam "deleted" LocalTime
    :> QueryParam "updated" LocalTime
    :> Get '[JSON] [FileKeyValue]

type GroupAPI
  = "groups"
    :> QueryParam "_id" GroupId
    :> QueryParam "encoding" Text
    :> QueryParam "is_admin" Bool
    :> QueryParam "name" Text
    :> QueryParam "created" LocalTime
    :> QueryParam "deleted" LocalTime
    :> QueryParam "updated" LocalTime
    :> Get '[JSON] [Group]

type InstitutionAPI
  = "institutions"
    :> QueryParam "_id" JournalId
    :> QueryParam "association_cd" Text
    :> QueryParam "encoding" Text
    :> QueryParam "is_foreign" Bool
    :> QueryParam "name" Text
    :> QueryParam "created" LocalTime
    :> QueryParam "deleted" LocalTime
    :> QueryParam "updated" LocalTime
    :> Get '[JSON] [Institution]

type InstitutionPersonAPI
  = "institution_person"
    :> QueryParam "_id" InstitutionPersonId
    :> QueryParam "institution" InstitutionId
    :> QueryParam "person" UserId
    :> QueryParam "created" LocalTime
    :> QueryParam "deleted" LocalTime
    :> QueryParam "updated" LocalTime
    :> Get '[JSON] [InstitutionPerson]

type InstrumentAPI
  = "instruments"
    :> QueryParam "_id" InstrumentId
    :> QueryParam "active" Bool
    :> QueryParam "display_name" Text
    :> QueryParam "encoding" Text
    :> QueryParam "name" Text
    :> QueryParam "name_short" Text
    :> QueryParam "created" LocalTime
    :> QueryParam "deleted" LocalTime
    :> QueryParam "updated" LocalTime
    :> Get '[JSON] [Instrument]

type InstrumentCustodianAPI
  = "instrument_custodian"
    :> QueryParam "_id" InstrumentCustodianId
    :> QueryParam "custodian" UserId
    :> QueryParam "instrument" InstrumentId
    :> QueryParam "created" LocalTime
    :> QueryParam "deleted" LocalTime
    :> QueryParam "updated" LocalTime
    :> Get '[JSON] [InstrumentCustodian]

type InstrumentGroupAPI
  = "instrument_group"
    :> QueryParam "_id" InstrumentGroupId
    :> QueryParam "group" GroupId
    :> QueryParam "instrument" InstrumentId
    :> QueryParam "created" LocalTime
    :> QueryParam "deleted" LocalTime
    :> QueryParam "updated" LocalTime
    :> Get '[JSON] [InstrumentGroup]

type JournalAPI
  = "journals"
    :> QueryParam "_id" JournalId
    :> QueryParam "encoding" Text
    :> QueryParam "impact_factor" Double
    :> QueryParam "name" Text
    :> QueryParam "website_url" Text
    :> QueryParam "created" LocalTime
    :> QueryParam "deleted" LocalTime
    :> QueryParam "updated" LocalTime
    :> Get '[JSON] [Journal]

type KeyAPI
  = "keys"
    :> QueryParam "_id" KeyId
    :> QueryParam "encoding" Text
    :> QueryParam "key" Text
    :> QueryParam "created" LocalTime
    :> QueryParam "deleted" LocalTime
    :> QueryParam "updated" LocalTime
    :> Get '[JSON] [Key]

type KeywordAPI
  = "keywords"
    :> QueryParam "_id" KeywordId
    :> QueryParam "encoding" Text
    :> QueryParam "keyword" Text
    :> QueryParam "created" LocalTime
    :> QueryParam "deleted" LocalTime
    :> QueryParam "updated" LocalTime
    :> Get '[JSON] [Keyword]

type ProposalAPI
  = "proposals"
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
    :> Get '[JSON] [Proposal]

type ProposalInstrumentAPI
  = "proposal_instrument"
    :> QueryParam "_id" ProposalInstrumentId
    :> QueryParam "instrument" InstrumentId
    :> QueryParam "proposal" ProposalId
    :> QueryParam "created" LocalTime
    :> QueryParam "deleted" LocalTime
    :> QueryParam "updated" LocalTime
    :> Get '[JSON] [ProposalInstrument]

type ProposalParticipantAPI
  = "proposal_participant"
    :> QueryParam "_id" ProposalParticipantId
    :> QueryParam "person" UserId
    :> QueryParam "proposal" ProposalId
    :> QueryParam "created" LocalTime
    :> QueryParam "deleted" LocalTime
    :> QueryParam "updated" LocalTime
    :> Get '[JSON] [ProposalParticipant]

type TransactionAPI
  = "transactions"
    :> QueryParam "_id" TransactionId
    :> QueryParam "instrument" InstrumentId
    :> QueryParam "proposal" ProposalId
    :> QueryParam "submitter" UserId
    :> QueryParam "created" LocalTime
    :> QueryParam "deleted" LocalTime
    :> QueryParam "updated" LocalTime
    :> Get '[JSON] [Transaction]

type TransactionKeyValueAPI
  = "trans_key_value"
    :> QueryParam "_id" TransactionKeyValueId
    :> QueryParam "key" KeyId
    :> QueryParam "transaction" TransactionId
    :> QueryParam "value" ValueId
    :> QueryParam "created" LocalTime
    :> QueryParam "deleted" LocalTime
    :> QueryParam "updated" LocalTime
    :> Get '[JSON] [TransactionKeyValue]

type UserAPI
  = "users"
    :> QueryParam "_id" UserId
    :> QueryParam "encoding" Text
    :> QueryParam "first_name" Text
    :> QueryParam "last_name" Text
    :> QueryParam "middle_initial" Text
    :> QueryParam "network_id" NetworkId
    :> QueryParam "created" LocalTime
    :> QueryParam "deleted" LocalTime
    :> QueryParam "updated" LocalTime
    :> Get '[JSON] [User]

type UserGroupAPI
  = "user_group"
    :> QueryParam "_id" UserGroupId
    :> QueryParam "group" GroupId
    :> QueryParam "person" UserId
    :> QueryParam "created" LocalTime
    :> QueryParam "deleted" LocalTime
    :> QueryParam "updated" LocalTime
    :> Get '[JSON] [UserGroup]

type ValueAPI
  = "values"
    :> QueryParam "_id" ValueId
    :> QueryParam "encoding" Text
    :> QueryParam "value" Text
    :> QueryParam "created" LocalTime
    :> QueryParam "deleted" LocalTime
    :> QueryParam "updated" LocalTime
    :> Get '[JSON] [Value]

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
