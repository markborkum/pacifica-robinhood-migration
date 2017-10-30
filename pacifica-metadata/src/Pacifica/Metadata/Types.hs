{-# LANGUAGE  GeneralizedNewtypeDeriving #-}
{-# LANGUAGE  OverloadedStrings #-}
{-# LANGUAGE  RecordWildCards #-}

-- |
-- Module:      Pacifica.Metadata.Types
-- Copyright:   (c) 2017 Pacific Northwest National Laboratory
-- License:     LGPL
-- Maintainer:  Mark Borkum <mark.borkum@pnnl.gov>
-- Stability:   experimental
-- Portability: portable
--
-- This module exports Haskell types for working with Pacifica Metadata Services.
--
module Pacifica.Metadata.Types where

import           Data.Aeson (FromJSON(parseJSON), ToJSON(toJSON), (.:), (.=))
import qualified Data.Aeson
import           Data.Text (Text)
import           Data.Time.Calendar (Day)
import           Data.Time.LocalTime (LocalTime)
import           Web.Internal.HttpApiData (FromHttpApiData(), ToHttpApiData())

newtype AnalyticalToolId = AnalyticalToolId { getAnalyticalToolId :: Int }
  deriving (Eq, Ord, Read, Show, FromJSON, ToJSON, FromHttpApiData, ToHttpApiData)

newtype AnalyticalToolProposalId = AnalyticalToolProposalId { getAnalyticalToolProposalId :: Text }
  deriving (Eq, Ord, Read, Show, FromJSON, ToJSON, FromHttpApiData, ToHttpApiData)

newtype AnalyticalToolTransactionId = AnalyticalToolTransactionId { getAnalyticalToolTransactionId :: Text }
  deriving (Eq, Ord, Read, Show, FromJSON, ToJSON, FromHttpApiData, ToHttpApiData)

newtype CitationId = CitationId { getCitationId :: Int }
  deriving (Eq, Ord, Read, Show, FromJSON, ToJSON, FromHttpApiData, ToHttpApiData)

newtype CitationContributorId = CitationContributorId { getCitationContributorId :: Text }
  deriving (Eq, Ord, Read, Show, FromJSON, ToJSON, FromHttpApiData, ToHttpApiData)

newtype CitationKeywordId = CitationKeywordId { getCitationKeywordId :: Text }
  deriving (Eq, Ord, Read, Show, FromJSON, ToJSON, FromHttpApiData, ToHttpApiData)

newtype CitationProposalId = CitationProposalId { getCitationProposalId :: Text }
  deriving (Eq, Ord, Read, Show, FromJSON, ToJSON, FromHttpApiData, ToHttpApiData)

newtype ContributorId = ContributorId { getContributorId :: Int }
  deriving (Eq, Ord, Read, Show, FromJSON, ToJSON, FromHttpApiData, ToHttpApiData)

newtype FileId = FileId { getFileId :: Int }
  deriving (Eq, Ord, Read, Show, FromJSON, ToJSON, FromHttpApiData, ToHttpApiData)

newtype FileKeyValueId = FileKeyValueId { getFileKeyValueId :: Text }
  deriving (Eq, Ord, Read, Show, FromJSON, ToJSON, FromHttpApiData, ToHttpApiData)

newtype GroupId = GroupId { getGroupId :: Int }
  deriving (Eq, Ord, Read, Show, FromJSON, ToJSON, FromHttpApiData, ToHttpApiData)

newtype InstitutionId = InstitutionId { getInstitutionId :: Int }
  deriving (Eq, Ord, Read, Show, FromJSON, ToJSON, FromHttpApiData, ToHttpApiData)

newtype InstitutionPersonId = InstitutionPersonId { getInstitutionPersonId :: Text }
  deriving (Eq, Ord, Read, Show, FromJSON, ToJSON, FromHttpApiData, ToHttpApiData)

newtype InstrumentId = InstrumentId { getInstrumentId :: Int }
  deriving (Eq, Ord, Read, Show, FromJSON, ToJSON, FromHttpApiData, ToHttpApiData)

newtype InstrumentCustodianId = InstrumentCustodianId { getInstrumentCustodianId :: Text }
  deriving (Eq, Ord, Read, Show, FromJSON, ToJSON, FromHttpApiData, ToHttpApiData)

newtype InstrumentGroupId = InstrumentGroupId { getInstrumentGroupId :: Text }
  deriving (Eq, Ord, Read, Show, FromJSON, ToJSON, FromHttpApiData, ToHttpApiData)

newtype JournalId = JournalId { getJournalId :: Int }
  deriving (Eq, Ord, Read, Show, FromJSON, ToJSON, FromHttpApiData, ToHttpApiData)

newtype KeyId = KeyId { getKeyId :: Int }
  deriving (Eq, Ord, Read, Show, FromJSON, ToJSON, FromHttpApiData, ToHttpApiData)

newtype KeywordId = KeywordId { getKeywordId :: Int }
  deriving (Eq, Ord, Read, Show, FromJSON, ToJSON, FromHttpApiData, ToHttpApiData)

newtype NetworkId = NetworkId { getNetworkId :: Text }
  deriving (Eq, Ord, Read, Show, FromJSON, ToJSON, FromHttpApiData, ToHttpApiData)

newtype ProposalId = ProposalId { getProposalId :: Text }
  deriving (Eq, Ord, Read, Show, FromJSON, ToJSON, FromHttpApiData, ToHttpApiData)

newtype ProposalInstrumentId = ProposalInstrumentId { getProposalInstrumentId :: Text }
  deriving (Eq, Ord, Read, Show, FromJSON, ToJSON, FromHttpApiData, ToHttpApiData)

newtype ProposalParticipantId = ProposalParticipantId { getProposalParticipantId :: Text }
  deriving (Eq, Ord, Read, Show, FromJSON, ToJSON, FromHttpApiData, ToHttpApiData)

newtype ReleaseAuthorizationId = ReleaseAuthorizationId { getReleaseAuthorizationId :: Text }
  deriving (Eq, Ord, Read, Show, FromJSON, ToJSON, FromHttpApiData, ToHttpApiData)

newtype TransactionId = TransactionId { getTransactionId :: Int }
  deriving (Eq, Ord, Read, Show, FromJSON, ToJSON, FromHttpApiData, ToHttpApiData)

newtype TransactionKeyValueId = TransactionKeyValueId { getTransactionKeyValueId :: Text }
  deriving (Eq, Ord, Read, Show, FromJSON, ToJSON, FromHttpApiData, ToHttpApiData)

newtype UserId = UserId { getUserId :: Int }
  deriving (Eq, Ord, Read, Show, FromJSON, ToJSON, FromHttpApiData, ToHttpApiData)

newtype UserGroupId = UserGroupId { getUserGroupId :: Text }
  deriving (Eq, Ord, Read, Show, FromJSON, ToJSON, FromHttpApiData, ToHttpApiData)

newtype ValueId = ValueId { getValueId :: Int }
  deriving (Eq, Ord, Read, Show, FromJSON, ToJSON, FromHttpApiData, ToHttpApiData)

data AnalyticalTool = AnalyticalTool
  { _analyticalToolId :: AnalyticalToolId
  , _analyticalToolEncoding :: Text
  , _analyticalToolName :: Text
  , _analyticalToolCreated :: LocalTime
  , _analyticalToolDeleted :: Maybe LocalTime
  , _analyticalToolUpdated :: LocalTime
  } deriving (Eq, Ord, Read, Show)

instance FromJSON AnalyticalTool where
  parseJSON = Data.Aeson.withObject "AnalyticalTool" $ \v -> pure AnalyticalTool
    <*> v .: "_id"
    <*> v .: "encoding"
    <*> v .: "name"
    <*> v .: "created"
    <*> v .: "deleted"
    <*> v .: "updated"
  {-# INLINE  parseJSON #-}

instance ToJSON AnalyticalTool where
  toJSON AnalyticalTool{..} = Data.Aeson.object
    [ "_id"      .= _analyticalToolId
    , "encoding" .= _analyticalToolEncoding
    , "name"     .= _analyticalToolName
    , "created"  .= _analyticalToolCreated
    , "updated"  .= _analyticalToolUpdated
    , "deleted"  .= _analyticalToolDeleted
    ]
  {-# INLINE  toJSON #-}

data AnalyticalToolProposal = AnalyticalToolProposal
  { _analyticalToolProposalId :: AnalyticalToolProposalId
  , _analyticalToolProposalAnalyticalToolId :: AnalyticalToolId
  , _analyticalToolProposalProposalId :: ProposalId
  , _analyticalToolProposalCreated :: LocalTime
  , _analyticalToolProposalDeleted :: Maybe LocalTime
  , _analyticalToolProposalUpdated :: LocalTime
  } deriving (Eq, Ord, Read, Show)

instance FromJSON AnalyticalToolProposal where
  parseJSON = Data.Aeson.withObject "AnalyticalToolProposal" $ \v -> pure AnalyticalToolProposal
    <*> v .: "_id"
    <*> v .: "analytical_tool_id"
    <*> v .: "proposal_id"
    <*> v .: "created"
    <*> v .: "deleted"
    <*> v .: "updated"
  {-# INLINE  parseJSON #-}

instance ToJSON AnalyticalToolProposal where
  toJSON AnalyticalToolProposal{..} = Data.Aeson.object
    [ "_id"                .= _analyticalToolProposalId
    , "analytical_tool_id" .= _analyticalToolProposalAnalyticalToolId
    , "proposal_id"        .= _analyticalToolProposalProposalId
    , "created"            .= _analyticalToolProposalCreated
    , "deleted"            .= _analyticalToolProposalDeleted
    , "updated"            .= _analyticalToolProposalUpdated
    ]
  {-# INLINE  toJSON #-}

data AnalyticalToolTransaction = AnalyticalToolTransaction
  { _analyticalToolTransactionId :: AnalyticalToolTransactionId
  , _analyticalToolTransactionAnalyticalToolId :: AnalyticalToolId
  , _analyticalToolTransactionTransactionId :: TransactionId
  , _analyticalToolTransactionCreated :: LocalTime
  , _analyticalToolTransactionDeleted :: Maybe LocalTime
  , _analyticalToolTransactionUpdated :: LocalTime
  } deriving (Eq, Ord, Read, Show)

instance FromJSON AnalyticalToolTransaction where
  parseJSON = Data.Aeson.withObject "AnalyticalToolTransaction" $ \v -> pure AnalyticalToolTransaction
    <*> v .: "_id"
    <*> v .: "analytical_tool_id"
    <*> v .: "transaction_id"
    <*> v .: "created"
    <*> v .: "deleted"
    <*> v .: "updated"
  {-# INLINE  parseJSON #-}

instance ToJSON AnalyticalToolTransaction where
  toJSON AnalyticalToolTransaction{..} = Data.Aeson.object
    [ "_id"                .= _analyticalToolTransactionId
    , "analytical_tool_id" .= _analyticalToolTransactionAnalyticalToolId
    , "transaction_id"     .= _analyticalToolTransactionTransactionId
    , "created"            .= _analyticalToolTransactionCreated
    , "deleted"            .= _analyticalToolTransactionDeleted
    , "updated"            .= _analyticalToolTransactionUpdated
    ]
  {-# INLINE  toJSON #-}

data Citation = Citation
  { _citationId :: CitationId
  , _citationAbstractText :: Text
  , _citationArticleTitle :: Text
  , _citationDOIReference :: Text
  , _citationEncoding :: Text
  , _citationJournalId :: JournalId
  , _citationJournalIssue :: Int
  , _citationJournalVolume :: Int
  , _citationPageRange :: Text
  , _citationReleaseAuthorizationId :: ReleaseAuthorizationId
  , _citationXMLText :: Text
  , _citationCreated :: LocalTime
  , _citationDeleted :: Maybe LocalTime
  , _citationUpdated :: LocalTime
  } deriving (Eq, Ord, Read, Show)

instance FromJSON Citation where
  parseJSON = Data.Aeson.withObject "Citation" $ \v -> pure Citation
    <*> v .: "_id"
    <*> v .: "abstract_text"
    <*> v .: "article_title"
    <*> v .: "doi_reference"
    <*> v .: "encoding"
    <*> v .: "journal_id"
    <*> v .: "journal_issue"
    <*> v .: "journal_volume"
    <*> v .: "page_range"
    <*> v .: "release_authorization_id"
    <*> v .: "xml_text"
    <*> v .: "created"
    <*> v .: "deleted"
    <*> v .: "updated"
  {-# INLINE  parseJSON #-}

instance ToJSON Citation where
  toJSON Citation{..} = Data.Aeson.object
    [ "_id"                      .= _citationId
    , "abstract_text"            .= _citationAbstractText
    , "article_title"            .= _citationArticleTitle
    , "doi_reference"            .= _citationDOIReference
    , "encoding"                 .= _citationEncoding
    , "journal_id"               .= _citationJournalId
    , "journal_issue"            .= _citationJournalIssue
    , "journal_volume"           .= _citationJournalVolume
    , "page_range"               .= _citationPageRange
    , "release_authorization_id" .= _citationReleaseAuthorizationId
    , "xml_text"                 .= _citationXMLText
    , "created"                  .= _citationCreated
    , "deleted"                  .= _citationDeleted
    , "updated"                  .= _citationUpdated
    ]
  {-# INLINE  toJSON #-}

data CitationContributor = CitationContributor
  { _citationContributorId :: CitationContributorId
  , _citationContributorAuthorId :: ContributorId
  , _citationContributorAuthorPrecedence :: Int
  , _citationContributorCitationId :: CitationId
  , _citationContributorCreated :: LocalTime
  , _citationContributorDeleted :: Maybe LocalTime
  , _citationContributorUpdated :: LocalTime
  } deriving (Eq, Ord, Read, Show)

instance FromJSON CitationContributor where
  parseJSON = Data.Aeson.withObject "CitationContributor" $ \v -> pure CitationContributor
    <*> v .: "_id"
    <*> v .: "author_id"
    <*> v .: "author_precedence"
    <*> v .: "citation_id"
    <*> v .: "created"
    <*> v .: "deleted"
    <*> v .: "updated"
  {-# INLINE  parseJSON #-}

instance ToJSON CitationContributor where
  toJSON CitationContributor{..} = Data.Aeson.object
    [ "_id"               .= _citationContributorId
    , "author_id"         .= _citationContributorAuthorId
    , "author_precedence" .= _citationContributorAuthorPrecedence
    , "citation_id"       .= _citationContributorCitationId
    , "created"           .= _citationContributorCreated
    , "deleted"           .= _citationContributorDeleted
    , "updated"           .= _citationContributorUpdated
    ]
  {-# INLINE  toJSON #-}

data CitationKeyword = CitationKeyword
  { _citationKeywordId :: CitationKeywordId
  , _citationKeywordCitationId :: CitationId
  , _citationKeywordKeywordId :: KeywordId
  , _citationKeywordCreated :: LocalTime
  , _citationKeywordDeleted :: Maybe LocalTime
  , _citationKeywordUpdated :: LocalTime
  } deriving (Eq, Ord, Read, Show)

instance FromJSON CitationKeyword where
  parseJSON = Data.Aeson.withObject "CitationKeyword" $ \v -> pure CitationKeyword
    <*> v .: "_id"
    <*> v .: "citation_id"
    <*> v .: "keyword_id"
    <*> v .: "created"
    <*> v .: "deleted"
    <*> v .: "updated"
  {-# INLINE  parseJSON #-}

instance ToJSON CitationKeyword where
  toJSON CitationKeyword{..} = Data.Aeson.object
    [ "_id"         .= _citationKeywordId
    , "citation_id" .= _citationKeywordCitationId
    , "keyword_id"  .= _citationKeywordKeywordId
    , "created"     .= _citationKeywordCreated
    , "deleted"     .= _citationKeywordDeleted
    , "updated"     .= _citationKeywordUpdated
    ]
  {-# INLINE  toJSON #-}

data CitationProposal = CitationProposal
  { _citationProposalId :: CitationProposalId
  , _citationProposalCitationId :: CitationId
  , _citationProposalProposalId :: ProposalId
  , _citationProposalCreated :: LocalTime
  , _citationProposalDeleted :: Maybe LocalTime
  , _citationProposalUpdated :: LocalTime
  } deriving (Eq, Ord, Read, Show)

instance FromJSON CitationProposal where
  parseJSON = Data.Aeson.withObject "CitationProposal" $ \v -> pure CitationProposal
    <*> v .: "_id"
    <*> v .: "citation_id"
    <*> v .: "proposal_id"
    <*> v .: "created"
    <*> v .: "deleted"
    <*> v .: "updated"
  {-# INLINE  parseJSON #-}

instance ToJSON CitationProposal where
  toJSON CitationProposal{..} = Data.Aeson.object
    [ "_id"         .= _citationProposalId
    , "citation_id" .= _citationProposalCitationId
    , "proposal_id" .= _citationProposalProposalId
    , "created"     .= _citationProposalCreated
    , "deleted"     .= _citationProposalDeleted
    , "updated"     .= _citationProposalUpdated
    ]
  {-# INLINE  toJSON #-}

data Contributor = Contributor
  { _contributorId :: ContributorId
  , _contributorDeptCode :: Text
  , _contributorEncoding :: Text
  , _contributorFirstName :: Text
  , _contributorInstitutionId :: InstitutionId
  , _contributorLastName :: Text
  , _contributorMiddleInitial :: Text
  , _contributorPersonId :: UserId
  , _contributorCreated :: LocalTime
  , _contributorDeleted :: Maybe LocalTime
  , _contributorUpdated :: LocalTime
  } deriving (Eq, Ord, Read, Show)

instance FromJSON Contributor where
  parseJSON = Data.Aeson.withObject "Contributor" $ \v -> pure Contributor
    <*> v .: "_id"
    <*> v .: "dept_code"
    <*> v .: "encoding"
    <*> v .: "first_name"
    <*> v .: "institution_id"
    <*> v .: "last_name"
    <*> v .: "middle_initial"
    <*> v .: "person_id"
    <*> v .: "created"
    <*> v .: "deleted"
    <*> v .: "updated"
  {-# INLINE  parseJSON #-}

instance ToJSON Contributor where
  toJSON Contributor{..} = Data.Aeson.object
    [ "_id"            .= _contributorId
    , "dept_code"      .= _contributorDeptCode
    , "encoding"       .= _contributorEncoding
    , "first_name"     .= _contributorFirstName
    , "institution_id" .= _contributorInstitutionId
    , "last_name"      .= _contributorLastName
    , "middle_initial" .= _contributorMiddleInitial
    , "person_id"      .= _contributorPersonId
    , "created"        .= _contributorCreated
    , "deleted"        .= _contributorDeleted
    , "updated"        .= _contributorUpdated
    ]
  {-# INLINE  toJSON #-}

data File = File
  { _fileId :: FileId
  , _fileChangeTime :: LocalTime
  , _fileEncoding :: Text
  , _fileHashSum :: Text
  , _fileHashType :: Text
  , _fileMIMEType :: Text
  , _fileModificationTime :: LocalTime
  , _fileName :: Text
  , _fileSize :: Int
  , _fileSubDirectory :: Text
  , _fileTransactionId :: TransactionId
  , _fileCreated :: LocalTime
  , _fileDeleted :: Maybe LocalTime
  , _fileUpdated :: LocalTime
  } deriving (Eq, Ord, Read, Show)

instance FromJSON File where
  parseJSON = Data.Aeson.withObject "File" $ \v -> pure File
    <*> v .: "_id"
    <*> v .: "ctime"
    <*> v .: "encoding"
    <*> v .: "hashsum"
    <*> v .: "hashtype"
    <*> v .: "mimetype"
    <*> v .: "mtime"
    <*> v .: "name"
    <*> v .: "size"
    <*> v .: "subdir"
    <*> v .: "transaction_id"
    <*> v .: "created"
    <*> v .: "deleted"
    <*> v .: "updated"
  {-# INLINE  parseJSON #-}

instance ToJSON File where
  toJSON File{..} = Data.Aeson.object
    [ "_id"            .= _fileId
    , "ctime"          .= _fileChangeTime
    , "encoding"       .= _fileEncoding
    , "hashsum"        .= _fileHashSum
    , "hashtype"       .= _fileHashType
    , "mimetype"       .= _fileMIMEType
    , "mtime"          .= _fileModificationTime
    , "name"           .= _fileName
    , "size"           .= _fileSize
    , "subdir"         .= _fileSubDirectory
    , "transaction_id" .= _fileTransactionId
    , "created"        .= _fileCreated
    , "deleted"        .= _fileDeleted
    , "updated"        .= _fileUpdated
    ]
  {-# INLINE  toJSON #-}

data FileKeyValue = FileKeyValue
  { _fileKeyValueId :: FileKeyValueId
  , _fileKeyValueFileId :: FileId
  , _fileKeyValueKeyId :: KeyId
  , _fileKeyValueValueId :: ValueId
  , _fileKeyValueCreated :: LocalTime
  , _fileKeyValueDeleted :: Maybe LocalTime
  , _fileKeyValueUpdated :: LocalTime
  } deriving (Eq, Ord, Read, Show)

instance FromJSON FileKeyValue where
  parseJSON = Data.Aeson.withObject "FileKeyValue" $ \v -> pure FileKeyValue
    <*> v .: "_id"
    <*> v .: "file_id"
    <*> v .: "key_id"
    <*> v .: "value_id"
    <*> v .: "created"
    <*> v .: "deleted"
    <*> v .: "updated"
  {-# INLINE  parseJSON #-}

instance ToJSON FileKeyValue where
  toJSON FileKeyValue{..} = Data.Aeson.object
    [ "_id"      .= _fileKeyValueId
    , "file_id"  .= _fileKeyValueFileId
    , "key_id"   .= _fileKeyValueKeyId
    , "value_id" .= _fileKeyValueValueId
    , "created"  .= _fileKeyValueCreated
    , "deleted"  .= _fileKeyValueDeleted
    , "updated"  .= _fileKeyValueUpdated
    ]
  {-# INLINE  toJSON #-}

data Group = Group
  { _groupId :: GroupId
  , _groupEncoding :: Text
  , _groupIsAdmin :: Bool
  , _groupName :: Text
  , _groupCreated :: LocalTime
  , _groupDeleted :: Maybe LocalTime
  , _groupUpdated :: LocalTime
  } deriving (Eq, Ord, Read, Show)

instance FromJSON Group where
  parseJSON = Data.Aeson.withObject "Group" $ \v -> pure Group
    <*> v .: "_id"
    <*> v .: "encoding"
    <*> v .: "is_admin"
    <*> v .: "name"
    <*> v .: "created"
    <*> v .: "deleted"
    <*> v .: "updated"
  {-# INLINE  parseJSON #-}

instance ToJSON Group where
  toJSON Group{..} = Data.Aeson.object
    [ "_id"      .= _groupId
    , "encoding" .= _groupEncoding
    , "is_admin" .= _groupIsAdmin
    , "name"     .= _groupName
    , "created"  .= _groupCreated
    , "deleted"  .= _groupDeleted
    , "updated"  .= _groupUpdated
    ]
  {-# INLINE  toJSON #-}

data Institution = Institution
  { _institutionId :: InstitutionId
  , _institutionAssociationCD :: Text
  , _institutionEncoding :: Text
  , _institutionIsForeign :: Bool
  , _institutionName :: Text
  , _institutionCreated :: LocalTime
  , _institutionDeleted :: Maybe LocalTime
  , _institutionUpdated :: LocalTime
  } deriving (Eq, Ord, Read, Show)

instance FromJSON Institution where
  parseJSON = Data.Aeson.withObject "Institution" $ \v -> pure Institution
    <*> v .: "_id"
    <*> v .: "association_cd"
    <*> v .: "encoding"
    <*> v .: "is_foreign"
    <*> v .: "name"
    <*> v .: "created"
    <*> v .: "deleted"
    <*> v .: "updated"
  {-# INLINE  parseJSON #-}

instance ToJSON Institution where
  toJSON Institution{..} = Data.Aeson.object
    [ "_id"            .= _institutionId
    , "association_cd" .= _institutionAssociationCD
    , "encoding"       .= _institutionEncoding
    , "is_foreign"     .= _institutionIsForeign
    , "name"           .= _institutionName
    , "created"        .= _institutionCreated
    , "deleted"        .= _institutionDeleted
    , "updated"        .= _institutionUpdated
    ]
  {-# INLINE  toJSON #-}

data InstitutionPerson = InstitutionPerson
  { _institutionPersonId :: InstitutionPersonId
  , _institutionPersonInstitutionId :: InstitutionId
  , _institutionPersonPersonId :: UserId
  , _institutionPersonCreated :: LocalTime
  , _institutionPersonDeleted :: Maybe LocalTime
  , _institutionPersonUpdated :: LocalTime
  } deriving (Eq, Ord, Read, Show)

instance FromJSON InstitutionPerson where
  parseJSON = Data.Aeson.withObject "InstitutionPerson" $ \v -> pure InstitutionPerson
    <*> v .: "_id"
    <*> v .: "institution_id"
    <*> v .: "person_id"
    <*> v .: "created"
    <*> v .: "deleted"
    <*> v .: "updated"
  {-# INLINE  parseJSON #-}

instance ToJSON InstitutionPerson where
  toJSON InstitutionPerson{..} = Data.Aeson.object
    [ "_id"            .= _institutionPersonId
    , "institution_id" .= _institutionPersonInstitutionId
    , "person_id"      .= _institutionPersonPersonId
    , "created"        .= _institutionPersonCreated
    , "updated"        .= _institutionPersonUpdated
    , "deleted"        .= _institutionPersonDeleted
    ]
  {-# INLINE  toJSON #-}

data Instrument = Instrument
  { _instrumentId :: InstrumentId
  , _instrumentActive :: Bool
  , _instrumentDisplayName :: Text
  , _instrumentEncoding :: Text
  , _instrumentName :: Text
  , _instrumentNameShort :: Text
  , _instrumentCreated :: LocalTime
  , _instrumentDeleted :: Maybe LocalTime
  , _instrumentUpdated :: LocalTime
  } deriving (Eq, Ord, Read, Show)

instance FromJSON Instrument where
  parseJSON = Data.Aeson.withObject "Instrument" $ \v -> pure Instrument
    <*> v .: "_id"
    <*> v .: "active"
    <*> v .: "display_name"
    <*> v .: "encoding"
    <*> v .: "name"
    <*> v .: "name_short"
    <*> v .: "created"
    <*> v .: "deleted"
    <*> v .: "updated"
  {-# INLINE  parseJSON #-}

instance ToJSON Instrument where
  toJSON Instrument{..} = Data.Aeson.object
    [ "_id"          .= _instrumentId
    , "active"       .= _instrumentActive
    , "display_name" .= _instrumentDisplayName
    , "encoding"     .= _instrumentEncoding
    , "name"         .= _instrumentName
    , "name_short"   .= _instrumentNameShort
    , "created"      .= _instrumentCreated
    , "deleted"      .= _instrumentDeleted
    , "updated"      .= _instrumentUpdated
    ]
  {-# INLINE  toJSON #-}

data InstrumentCustodian = InstrumentCustodian
  { _instrumentCustodianId :: InstrumentCustodianId
  , _instrumentCustodianCustodianId :: UserId
  , _instrumentCustodianInstrumentId :: InstrumentId
  , _instrumentCustodianCreated :: LocalTime
  , _instrumentCustodianDeleted :: Maybe LocalTime
  , _instrumentCustodianUpdated :: LocalTime
  } deriving (Eq, Ord, Read, Show)

instance FromJSON InstrumentCustodian where
  parseJSON = Data.Aeson.withObject "InstrumentCustodian" $ \v -> pure InstrumentCustodian
    <*> v .: "_id"
    <*> v .: "custodian_id"
    <*> v .: "instrument_id"
    <*> v .: "created"
    <*> v .: "deleted"
    <*> v .: "updated"
  {-# INLINE  parseJSON #-}

instance ToJSON InstrumentCustodian where
  toJSON InstrumentCustodian{..} = Data.Aeson.object
    [ "_id"           .= _instrumentCustodianId
    , "custodian_id"  .= _instrumentCustodianCustodianId
    , "instrument_id" .= _instrumentCustodianInstrumentId
    , "created"       .= _instrumentCustodianCreated
    , "deleted"       .= _instrumentCustodianDeleted
    , "updated"       .= _instrumentCustodianUpdated
    ]
  {-# INLINE  toJSON #-}

data InstrumentGroup = InstrumentGroup
  { _instrumentGroupId :: InstrumentGroupId
  , _instrumentGroupGroupId :: GroupId
  , _instrumentGroupInstrumentId :: InstrumentId
  , _instrumentGroupCreated :: LocalTime
  , _instrumentGroupDeleted :: Maybe LocalTime
  , _instrumentGroupUpdated :: LocalTime
  } deriving (Eq, Ord, Read, Show)

instance FromJSON InstrumentGroup where
  parseJSON = Data.Aeson.withObject "InstrumentGroup" $ \v -> pure InstrumentGroup
    <*> v .: "_id"
    <*> v .: "group_id"
    <*> v .: "instrument_id"
    <*> v .: "created"
    <*> v .: "deleted"
    <*> v .: "updated"
  {-# INLINE  parseJSON #-}

instance ToJSON InstrumentGroup where
  toJSON InstrumentGroup{..} = Data.Aeson.object
    [ "_id"           .= _instrumentGroupId
    , "group_id"      .= _instrumentGroupGroupId
    , "instrument_id" .= _instrumentGroupInstrumentId
    , "created"       .= _instrumentGroupCreated
    , "deleted"       .= _instrumentGroupDeleted
    , "updated"       .= _instrumentGroupUpdated
    ]
  {-# INLINE  toJSON #-}

data Journal = Journal
  { _journalId :: JournalId
  , _journalEncoding :: Text
  , _journalImpactFactor :: Double
  , _journalName :: Text
  , _journalWebsiteURL :: Text
  , _journalCreated :: LocalTime
  , _journalDeleted :: Maybe LocalTime
  , _journalUpdated :: LocalTime
  } deriving (Eq, Ord, Read, Show)

instance FromJSON Journal where
  parseJSON = Data.Aeson.withObject "Journal" $ \v -> pure Journal
    <*> v .: "_id"
    <*> v .: "encoding"
    <*> v .: "impact_factor"
    <*> v .: "name"
    <*> v .: "website_url"
    <*> v .: "created"
    <*> v .: "deleted"
    <*> v .: "updated"
  {-# INLINE  parseJSON #-}

instance ToJSON Journal where
  toJSON Journal{..} = Data.Aeson.object
    [ "_id"           .= _journalId
    , "encoding"      .= _journalEncoding
    , "impact_factor" .= _journalImpactFactor
    , "name"          .= _journalName
    , "website_url"   .= _journalWebsiteURL
    , "created"       .= _journalCreated
    , "deleted"       .= _journalDeleted
    , "updated"       .= _journalUpdated
    ]
  {-# INLINE  toJSON #-}

data Key = Key
  { _keyId :: KeyId
  , _keyEncoding :: Text
  , _keyKey :: Text
  , _keyCreated :: LocalTime
  , _keyDeleted :: Maybe LocalTime
  , _keyUpdated :: LocalTime
  } deriving (Eq, Ord, Read, Show)

instance FromJSON Key where
  parseJSON = Data.Aeson.withObject "Key" $ \v -> pure Key
    <*> v .: "_id"
    <*> v .: "encoding"
    <*> v .: "key"
    <*> v .: "created"
    <*> v .: "deleted"
    <*> v .: "updated"
  {-# INLINE  parseJSON #-}

instance ToJSON Key where
  toJSON Key{..} = Data.Aeson.object
    [ "_id"      .= _keyId
    , "encoding" .= _keyEncoding
    , "key"      .= _keyKey
    , "created"  .= _keyCreated
    , "deleted"  .= _keyDeleted
    , "updated"  .= _keyUpdated
    ]
  {-# INLINE  toJSON #-}

data Keyword = Keyword
  { _keywordId :: KeywordId
  , _keywordEncoding :: Text
  , _keywordKeyword :: Text
  , _keywordCreated :: LocalTime
  , _keywordDeleted :: Maybe LocalTime
  , _keywordUpdated :: LocalTime
  } deriving (Eq, Ord, Read, Show)

instance FromJSON Keyword where
  parseJSON = Data.Aeson.withObject "Keyword" $ \v -> pure Keyword
    <*> v .: "_id"
    <*> v .: "encoding"
    <*> v .: "keyword"
    <*> v .: "created"
    <*> v .: "deleted"
    <*> v .: "updated"
  {-# INLINE  parseJSON #-}

instance ToJSON Keyword where
  toJSON Keyword{..} = Data.Aeson.object
    [ "_id"      .= _keywordId
    , "encoding" .= _keywordEncoding
    , "keyword"  .= _keywordKeyword
    , "created"  .= _keywordCreated
    , "deleted"  .= _keywordDeleted
    , "updated"  .= _keywordUpdated
    ]
  {-# INLINE  toJSON #-}

data Proposal = Proposal
  { _proposalId :: ProposalId
  , _proposalAbstract :: Text
  , _proposalAcceptedDate :: Maybe Day
  , _proposalActualEndDate :: Maybe Day
  , _proposalActualStartDate :: Maybe Day
  , _proposalClosedDate :: Maybe Day
  , _proposalEncoding :: Text
  , _proposalProposalType :: Text
  , _proposalScienceTheme :: Maybe Text
  , _proposalSubmittedDate :: LocalTime
  , _proposalTitle :: Text
  , _proposalCreated :: LocalTime
  , _proposalDeleted :: Maybe LocalTime
  , _proposalUpdated :: LocalTime
  } deriving (Eq, Ord, Read, Show)

instance FromJSON Proposal where
  parseJSON = Data.Aeson.withObject "Proposal" $ \v -> pure Proposal
    <*> v .: "_id"
    <*> v .: "abstract"
    <*> v .: "accepted_date"
    <*> v .: "actual_end_date"
    <*> v .: "actual_start_date"
    <*> v .: "closed_date"
    <*> v .: "encoding"
    <*> v .: "proposal_type"
    <*> v .: "science_theme"
    <*> v .: "submitted_date"
    <*> v .: "title"
    <*> v .: "created"
    <*> v .: "deleted"
    <*> v .: "updated"
  {-# INLINE  parseJSON #-}

instance ToJSON Proposal where
  toJSON Proposal{..} = Data.Aeson.object
    [ "_id"               .= _proposalId
    , "abstract"          .= _proposalAbstract
    , "accepted_date"     .= _proposalAcceptedDate
    , "actual_end_date"   .= _proposalActualEndDate
    , "actual_start_date" .= _proposalActualStartDate
    , "closed_date"       .= _proposalClosedDate
    , "encoding"          .= _proposalEncoding
    , "proposal_type"     .= _proposalProposalType
    , "science_theme"     .= _proposalScienceTheme
    , "submitted_date"    .= _proposalSubmittedDate
    , "title"             .= _proposalTitle
    , "created"           .= _proposalCreated
    , "deleted"           .= _proposalDeleted
    , "updated"           .= _proposalUpdated
    ]
  {-# INLINE  toJSON #-}

data ProposalInstrument = ProposalInstrument
  { _proposalInstrumentId :: ProposalInstrumentId
  , _proposalInstrumentInstrumentId :: InstrumentId
  , _proposalInstrumentProposalId :: ProposalId
  , _proposalInstrumentCreated :: LocalTime
  , _proposalInstrumentDeleted :: Maybe LocalTime
  , _proposalInstrumentUpdated :: LocalTime
  } deriving (Eq, Ord, Read, Show)

instance FromJSON ProposalInstrument where
  parseJSON = Data.Aeson.withObject "ProposalInstrument" $ \v -> pure ProposalInstrument
    <*> v .: "_id"
    <*> v .: "instrument_id"
    <*> v .: "proposal_id"
    <*> v .: "created"
    <*> v .: "deleted"
    <*> v .: "updated"
  {-# INLINE  parseJSON #-}

instance ToJSON ProposalInstrument where
  toJSON ProposalInstrument{..} = Data.Aeson.object
    [ "_id"           .= _proposalInstrumentId
    , "instrument_id" .= _proposalInstrumentInstrumentId
    , "proposal_id"   .= _proposalInstrumentProposalId
    , "created"       .= _proposalInstrumentCreated
    , "deleted"       .= _proposalInstrumentDeleted
    , "updated"       .= _proposalInstrumentUpdated
    ]
  {-# INLINE  toJSON #-}

data ProposalParticipant = ProposalParticipant
  { _proposalParticipantId :: ProposalParticipantId
  , _proposalParticipantPersonId :: UserId
  , _proposalParticipantProposalId :: ProposalId
  , _proposalParticipantCreated :: LocalTime
  , _proposalParticipantDeleted :: Maybe LocalTime
  , _proposalParticipantUpdated :: LocalTime
  } deriving (Eq, Ord, Read, Show)

instance FromJSON ProposalParticipant where
  parseJSON = Data.Aeson.withObject "ProposalParticipant" $ \v -> pure ProposalParticipant
    <*> v .: "_id"
    <*> v .: "person_id"
    <*> v .: "proposal_id"
    <*> v .: "created"
    <*> v .: "deleted"
    <*> v .: "updated"
  {-# INLINE  parseJSON #-}

instance ToJSON ProposalParticipant where
  toJSON ProposalParticipant{..} = Data.Aeson.object
    [ "_id"         .= _proposalParticipantId
    , "person_id"   .= _proposalParticipantPersonId
    , "proposal_id" .= _proposalParticipantProposalId
    , "created"     .= _proposalParticipantCreated
    , "deleted"     .= _proposalParticipantDeleted
    , "updated"     .= _proposalParticipantUpdated
    ]
  {-# INLINE  toJSON #-}

data Transaction = Transaction
  { _transactionId :: TransactionId
  , _transactionInstrumentId :: InstrumentId
  , _transactionProposalId :: ProposalId
  , _transactionSubmitterId :: UserId
  , _transactionCreated :: LocalTime
  , _transactionDeleted :: Maybe LocalTime
  , _transactionUpdated :: LocalTime
  } deriving (Eq, Ord, Read, Show)

instance FromJSON Transaction where
  parseJSON = Data.Aeson.withObject "Transaction" $ \v -> pure Transaction
    <*> v .: "_id"
    <*> v .: "instrument_id"
    <*> v .: "proposal_id"
    <*> v .: "submitter_id"
    <*> v .: "created"
    <*> v .: "deleted"
    <*> v .: "updated"
  {-# INLINE  parseJSON #-}

instance ToJSON Transaction where
  toJSON Transaction{..} = Data.Aeson.object
    [ "_id"           .= _transactionId
    , "instrument_id" .= _transactionInstrumentId
    , "proposal_id"   .= _transactionProposalId
    , "submitter_id"  .= _transactionSubmitterId
    , "created"       .= _transactionCreated
    , "deleted"       .= _transactionDeleted
    , "updated"       .= _transactionUpdated
    ]
  {-# INLINE  toJSON #-}

data TransactionKeyValue = TransactionKeyValue
  { _transactionKeyValueId :: TransactionKeyValueId
  , _transactionKeyValueKeyId :: KeyId
  , _transactionKeyValueTransactionId :: TransactionId
  , _transactionKeyValueValueId :: ValueId
  , _transactionKeyValueCreated :: LocalTime
  , _transactionKeyValueDeleted :: Maybe LocalTime
  , _transactionKeyValueUpdated :: LocalTime
  } deriving (Eq, Ord, Read, Show)

instance FromJSON TransactionKeyValue where
  parseJSON = Data.Aeson.withObject "TransactionKeyValue" $ \v -> pure TransactionKeyValue
    <*> v .: "_id"
    <*> v .: "key_id"
    <*> v .: "transaction_id"
    <*> v .: "value_id"
    <*> v .: "created"
    <*> v .: "deleted"
    <*> v .: "updated"
  {-# INLINE  parseJSON #-}

instance ToJSON TransactionKeyValue where
  toJSON TransactionKeyValue{..} = Data.Aeson.object
    [ "_id"            .= _transactionKeyValueId
    , "key_id"         .= _transactionKeyValueKeyId
    , "transaction_id" .= _transactionKeyValueTransactionId
    , "value_id"       .= _transactionKeyValueValueId
    , "created"        .= _transactionKeyValueCreated
    , "deleted"        .= _transactionKeyValueDeleted
    , "updated"        .= _transactionKeyValueUpdated
    ]
  {-# INLINE  toJSON #-}

data User = User
  { _userId :: UserId
  , _userEmailAddress :: Text
  , _userEncoding :: Text
  , _userFirstName :: Text
  , _userLastName :: Text
  , _userMiddleInitial :: Text
  , _userNetworkId :: Maybe NetworkId
  , _userCreated :: LocalTime
  , _userDeleted :: Maybe LocalTime
  , _userUpdated :: LocalTime
  } deriving (Eq, Ord, Read, Show)

instance FromJSON User where
  parseJSON = Data.Aeson.withObject "User" $ \v -> pure User
    <*> v .: "_id"
    <*> v .: "email_address"
    <*> v .: "encoding"
    <*> v .: "first_name"
    <*> v .: "last_name"
    <*> v .: "middle_initial"
    <*> v .: "network_id"
    <*> v .: "created"
    <*> v .: "deleted"
    <*> v .: "updated"
  {-# INLINE  parseJSON #-}

instance ToJSON User where
  toJSON User{..} = Data.Aeson.object
    [ "_id"            .= _userId
    , "email_address"  .= _userEmailAddress
    , "encoding"       .= _userEncoding
    , "first_name"     .= _userFirstName
    , "last_name"      .= _userLastName
    , "middle_initial" .= _userMiddleInitial
    , "network_id"     .= _userNetworkId
    , "created"        .= _userCreated
    , "deleted"        .= _userDeleted
    , "updated"        .= _userUpdated
    ]
  {-# INLINE  toJSON #-}

data UserGroup = UserGroup
  { _userGroupId :: UserGroupId
  , _userGroupGroupId :: GroupId
  , _userGroupPersonId :: UserId
  , _userGroupCreated :: LocalTime
  , _userGroupDeleted :: Maybe LocalTime
  , _userGroupUpdated :: LocalTime
  } deriving (Eq, Ord, Read, Show)

instance FromJSON UserGroup where
  parseJSON = Data.Aeson.withObject "UserGroup" $ \v -> pure UserGroup
    <*> v .: "_id"
    <*> v .: "group_id"
    <*> v .: "person_id"
    <*> v .: "created"
    <*> v .: "deleted"
    <*> v .: "updated"
  {-# INLINE  parseJSON #-}

instance ToJSON UserGroup where
  toJSON UserGroup{..} = Data.Aeson.object
    [ "_id"       .= _userGroupId
    , "group_id"  .= _userGroupGroupId
    , "person_id" .= _userGroupPersonId
    , "created"   .= _userGroupCreated
    , "deleted"   .= _userGroupDeleted
    , "updated"   .= _userGroupUpdated
    ]
  {-# INLINE  toJSON #-}

data Value = Value
  { _valueId :: ValueId
  , _valueEncoding :: Text
  , _valueValue :: Text
  , _valueCreated :: LocalTime
  , _valueDeleted :: Maybe LocalTime
  , _valueUpdated :: LocalTime
  } deriving (Eq, Ord, Read, Show)

instance FromJSON Value where
  parseJSON = Data.Aeson.withObject "Value" $ \v -> pure Value
    <*> v .: "_id"
    <*> v .: "encoding"
    <*> v .: "value"
    <*> v .: "created"
    <*> v .: "deleted"
    <*> v .: "updated"
  {-# INLINE  parseJSON #-}

instance ToJSON Value where
  toJSON Value{..} = Data.Aeson.object
    [ "_id"      .= _valueId
    , "encoding" .= _valueEncoding
    , "value"    .= _valueValue
    , "created"  .= _valueCreated
    , "deleted"  .= _valueDeleted
    , "updated"  .= _valueUpdated
    ]
  {-# INLINE  toJSON #-}
