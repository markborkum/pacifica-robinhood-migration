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
import           Servant.API.ContentTypes (JSON)
import           Servant.API.QueryParam (QueryParam)
import           Servant.API.Sub ((:>))
import           Servant.API.Verbs (Get)

-- | Servant API type for Pacifica Metadata Services.
--
type API
  = "analytical_tools"
    :> QueryParam "_id" AnalyticalToolId
    :> QueryParam "encoding" Text
    :> QueryParam "name" Text
    :> QueryParam "created" LocalTime
    :> QueryParam "deleted" LocalTime
    :> QueryParam "updated" LocalTime
    :> Get '[JSON] [AnalyticalTool]
  :<|> "atool_proposal"
    :> QueryParam "_id" AnalyticalToolProposalId
    :> QueryParam "analytical_tool" AnalyticalToolId
    :> QueryParam "proposal" ProposalId
    :> QueryParam "created" LocalTime
    :> QueryParam "deleted" LocalTime
    :> QueryParam "updated" LocalTime
    :> Get '[JSON] [AnalyticalToolProposal]
  :<|> "atool_transaction"
    :> QueryParam "_id" AnalyticalToolTransactionId
    :> QueryParam "analytical_tool" AnalyticalToolId
    :> QueryParam "transaction" TransactionId
    :> QueryParam "created" LocalTime
    :> QueryParam "deleted" LocalTime
    :> QueryParam "updated" LocalTime
    :> Get '[JSON] [AnalyticalToolTransaction]
  :<|> "citation_contributor"
    :> QueryParam "_id" CitationContributorId
    :> QueryParam "author" ContributorId
    :> QueryParam "author_precedence" Int
    :> QueryParam "citation" CitationId
    :> QueryParam "created" LocalTime
    :> QueryParam "deleted" LocalTime
    :> QueryParam "updated" LocalTime
    :> Get '[JSON] [CitationContributor]
  :<|> "citation_keyword"
    :> QueryParam "_id" CitationKeywordId
    :> QueryParam "citation" CitationId
    :> QueryParam "keyword" KeywordId
    :> QueryParam "created" LocalTime
    :> QueryParam "deleted" LocalTime
    :> QueryParam "updated" LocalTime
    :> Get '[JSON] [CitationKeyword]
  :<|> "citation_proposal"
    :> QueryParam "_id" CitationProposalId
    :> QueryParam "citation" CitationId
    :> QueryParam "proposal" ProposalId
    :> QueryParam "created" LocalTime
    :> QueryParam "deleted" LocalTime
    :> QueryParam "updated" LocalTime
    :> Get '[JSON] [CitationProposal]
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
    :> Get '[JSON] [Citation]
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
    :> Get '[JSON] [Contributor]
  :<|> "file_key_value"
    :> QueryParam "_id" FileKeyValueId
    :> QueryParam "file" FileId
    :> QueryParam "key" KeyId
    :> QueryParam "value" ValueId
    :> QueryParam "created" LocalTime
    :> QueryParam "deleted" LocalTime
    :> QueryParam "updated" LocalTime
    :> Get '[JSON] [FileKeyValue]
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
    :> Get '[JSON] [File]
  :<|> "groups"
    :> QueryParam "_id" GroupId
    :> QueryParam "encoding" Text
    :> QueryParam "is_admin" Bool
    :> QueryParam "name" Text
    :> QueryParam "created" LocalTime
    :> QueryParam "deleted" LocalTime
    :> QueryParam "updated" LocalTime
    :> Get '[JSON] [Group]
  :<|> "institution_person"
    :> QueryParam "_id" InstitutionPersonId
    :> QueryParam "institution" InstitutionId
    :> QueryParam "person" UserId
    :> QueryParam "created" LocalTime
    :> QueryParam "deleted" LocalTime
    :> QueryParam "updated" LocalTime
    :> Get '[JSON] [InstitutionPerson]
  :<|> "institutions"
    :> QueryParam "_id" JournalId
    :> QueryParam "association_cd" Text
    :> QueryParam "encoding" Text
    :> QueryParam "is_foreign" Bool
    :> QueryParam "name" Text
    :> QueryParam "created" LocalTime
    :> QueryParam "deleted" LocalTime
    :> QueryParam "updated" LocalTime
    :> Get '[JSON] [Institution]
  :<|> "instrument_custodian"
    :> QueryParam "_id" InstrumentCustodianId
    :> QueryParam "custodian" UserId
    :> QueryParam "instrument" InstrumentId
    :> QueryParam "created" LocalTime
    :> QueryParam "deleted" LocalTime
    :> QueryParam "updated" LocalTime
    :> Get '[JSON] [InstrumentCustodian]
  :<|> "instrument_group"
    :> QueryParam "_id" InstrumentGroupId
    :> QueryParam "group" GroupId
    :> QueryParam "instrument" InstrumentId
    :> QueryParam "created" LocalTime
    :> QueryParam "deleted" LocalTime
    :> QueryParam "updated" LocalTime
    :> Get '[JSON] [InstrumentGroup]
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
    :> Get '[JSON] [Instrument]
  :<|> "journals"
    :> QueryParam "_id" JournalId
    :> QueryParam "encoding" Text
    :> QueryParam "impact_factor" Double
    :> QueryParam "name" Text
    :> QueryParam "website_url" Text
    :> QueryParam "created" LocalTime
    :> QueryParam "deleted" LocalTime
    :> QueryParam "updated" LocalTime
    :> Get '[JSON] [Journal]
  :<|> "keys"
    :> QueryParam "_id" KeyId
    :> QueryParam "encoding" Text
    :> QueryParam "key" Text
    :> QueryParam "created" LocalTime
    :> QueryParam "deleted" LocalTime
    :> QueryParam "updated" LocalTime
    :> Get '[JSON] [Key]
  :<|> "keywords"
    :> QueryParam "_id" KeywordId
    :> QueryParam "encoding" Text
    :> QueryParam "keyword" Text
    :> QueryParam "created" LocalTime
    :> QueryParam "deleted" LocalTime
    :> QueryParam "updated" LocalTime
    :> Get '[JSON] [Keyword]
  :<|> "proposal_instrument"
    :> QueryParam "_id" ProposalInstrumentId
    :> QueryParam "instrument" InstrumentId
    :> QueryParam "proposal" ProposalId
    :> QueryParam "created" LocalTime
    :> QueryParam "deleted" LocalTime
    :> QueryParam "updated" LocalTime
    :> Get '[JSON] [ProposalInstrument]
  :<|> "proposal_participant"
    :> QueryParam "_id" ProposalParticipantId
    :> QueryParam "person" UserId
    :> QueryParam "proposal" ProposalId
    :> QueryParam "created" LocalTime
    :> QueryParam "deleted" LocalTime
    :> QueryParam "updated" LocalTime
    :> Get '[JSON] [ProposalParticipant]
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
    :> Get '[JSON] [Proposal]
  :<|> "trans_key_value"
    :> QueryParam "_id" TransactionKeyValueId
    :> QueryParam "key" KeyId
    :> QueryParam "transaction" TransactionId
    :> QueryParam "value" ValueId
    :> QueryParam "created" LocalTime
    :> QueryParam "deleted" LocalTime
    :> QueryParam "updated" LocalTime
    :> Get '[JSON] [TransactionKeyValue]
  :<|> "transactions"
    :> QueryParam "_id" TransactionId
    :> QueryParam "instrument" InstrumentId
    :> QueryParam "proposal" ProposalId
    :> QueryParam "submitter" UserId
    :> QueryParam "created" LocalTime
    :> QueryParam "deleted" LocalTime
    :> QueryParam "updated" LocalTime
    :> Get '[JSON] [Transaction]
  :<|> "user_group"
    :> QueryParam "_id" UserGroupId
    :> QueryParam "group" GroupId
    :> QueryParam "person" UserId
    :> QueryParam "created" LocalTime
    :> QueryParam "deleted" LocalTime
    :> QueryParam "updated" LocalTime
    :> Get '[JSON] [UserGroup]
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
    :> Get '[JSON] [User]
  :<|> "values"
    :> QueryParam "_id" ValueId
    :> QueryParam "encoding" Text
    :> QueryParam "value" Text
    :> QueryParam "created" LocalTime
    :> QueryParam "deleted" LocalTime
    :> QueryParam "updated" LocalTime
    :> Get '[JSON] [Value]

-- | Proxy for Servant API type for Pacifica Metadata Services.
--
api :: Proxy API
api = Proxy
{-# INLINE  api #-}
