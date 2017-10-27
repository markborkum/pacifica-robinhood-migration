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
-- >     mx = readUser' $ UserId 42
-- >   x <- runCurlClientM mx env
-- >   case x of
-- >     Left err -> putStr "Error: " >> print err
-- >     Right Nothing -> putStrLn "Warning: User not found"
-- >     Right (Just user) -> print user
--
module Pacifica.Metadata.API.Curl
  ( -- * API implementations
    -- ** AnalyticalTool type
    createAnalyticalTool , readAnalyticalTool , readAnalyticalTool' , updateAnalyticalTool , destroyAnalyticalTool
    -- ** AnalyticalToolProposal type
  , createAnalyticalToolProposal , readAnalyticalToolProposal , readAnalyticalToolProposal' , updateAnalyticalToolProposal , destroyAnalyticalToolProposal
    -- ** AnalyticalToolTransaction type
  , createAnalyticalToolTransaction , readAnalyticalToolTransaction , readAnalyticalToolTransaction' , updateAnalyticalToolTransaction , destroyAnalyticalToolTransaction
    -- ** Citation type
  , createCitation , readCitation , updateCitation , destroyCitation
    -- ** CitationContributor type
  , createCitationContributor , readCitationContributor , updateCitationContributor , destroyCitationContributor
    -- ** CitationKeyword type
  , createCitationKeyword , readCitationKeyword , updateCitationKeyword , destroyCitationKeyword
    -- ** CitationProposal type
  , createCitationProposal , readCitationProposal , updateCitationProposal , destroyCitationProposal
    -- ** Contributor type
  , createContributor , readContributor , updateContributor , destroyContributor
    -- ** File type
  , createFile , readFile , updateFile , destroyFile
    -- ** FileKeyValue type
  , createFileKeyValue , readFileKeyValue , updateFileKeyValue , destroyFileKeyValue
    -- ** Group type
  , createGroup , readGroup , updateGroup , destroyGroup
    -- ** Institution type
  , createInstitution , readInstitution , updateInstitution , destroyInstitution
    -- ** InstitutionPerson type
  , createInstitutionPerson , readInstitutionPerson , updateInstitutionPerson , destroyInstitutionPerson
    -- ** Instrument type
  , createInstrument , readInstrument , updateInstrument , destroyInstrument
    -- ** InstrumentCustodian type
  , createInstrumentCustodian , readInstrumentCustodian , updateInstrumentCustodian , destroyInstrumentCustodian
    -- ** InstrumentGroup type
  , createInstrumentGroup , readInstrumentGroup , updateInstrumentGroup , destroyInstrumentGroup
    -- ** Journal type
  , createJournal , readJournal , updateJournal , destroyJournal
    -- ** Key type
  , createKey , readKey , updateKey , destroyKey
    -- ** Keyword type
  , createKeyword , readKeyword , updateKeyword , destroyKeyword
    -- ** Proposal type
  , createProposal , readProposal , updateProposal , destroyProposal
    -- ** ProposalInstrument type
  , createProposalInstrument , readProposalInstrument , updateProposalInstrument , destroyProposalInstrument
    -- ** ProposalParticipant type
  , createProposalParticipant , readProposalParticipant , updateProposalParticipant , destroyProposalParticipant
    -- ** Transaction type
  , createTransaction , readTransaction , updateTransaction , destroyTransaction
    -- ** TransactionKeyValue type
  , createTransactionKeyValue , readTransactionKeyValue , updateTransactionKeyValue , destroyTransactionKeyValue
    -- ** User type
  , createUser , readUser , readUser' , updateUser , destroyUser
    -- ** UserGroup type
  , createUserGroup , readUserGroup , updateUserGroup , destroyUserGroup
    -- ** Value type
  , createValue , readValue , updateValue , destroyValue
  ) where

import           Data.Aeson (FromJSON(), ToJSON())
import qualified Data.Aeson
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy
import qualified Data.ByteString.Lazy.Char8
import           Data.Default (Default(def))
import qualified Data.List
import           Data.Proxy (Proxy(..))
import           Data.Text (Text)
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

readAnalyticalTool'
  :: ()
  => AnalyticalToolId -- ^ _id
  -> CurlClientM (Maybe AnalyticalTool)
readAnalyticalTool' analyticalToolId = safeHead <$> readAnalyticalTool (Just analyticalToolId) Nothing Nothing Nothing Nothing Nothing (Just 1) (Just 1)
{-# INLINE  readAnalyticalTool' #-}

createAnalyticalToolProposal
  :: ()
  => AnalyticalToolProposal -- ^ DATA
  -> CurlClientM NoContent
readAnalyticalToolProposal
  :: ()
  => Maybe AnalyticalToolProposalId -- ^ _id
  -> Maybe AnalyticalToolId -- ^ analytic_tool
  -> Maybe ProposalId -- ^ proposal
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

readAnalyticalToolProposal'
  :: ()
  => AnalyticalToolProposalId -- ^ _id
  -> CurlClientM (Maybe AnalyticalToolProposal)
readAnalyticalToolProposal' analyticalToolProposalId = safeHead <$> readAnalyticalToolProposal (Just analyticalToolProposalId) Nothing Nothing Nothing Nothing Nothing (Just 1) (Just 1)
{-# INLINE  readAnalyticalToolProposal' #-}

createAnalyticalToolTransaction
  :: ()
  => AnalyticalToolTransaction -- ^ DATA
  -> CurlClientM NoContent
readAnalyticalToolTransaction
  :: ()
  => Maybe AnalyticalToolTransactionId -- ^ _id
  -> Maybe AnalyticalToolId -- ^ analytic_tool
  -> Maybe TransactionId -- ^ transaction
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

readAnalyticalToolTransaction'
  :: ()
  => AnalyticalToolTransactionId -- ^ _id
  -> CurlClientM (Maybe AnalyticalToolTransaction)
readAnalyticalToolTransaction' analyticalToolTransactionId = safeHead <$> readAnalyticalToolTransaction (Just analyticalToolTransactionId) Nothing Nothing Nothing Nothing Nothing (Just 1) (Just 1)
{-# INLINE  readAnalyticalToolTransaction' #-}

createCitation :<|> readCitation :<|> updateCitation :<|> destroyCitation = toClient apiCitation def
{-# NOINLINE  createCitation #-}
{-# NOINLINE  readCitation #-}
{-# NOINLINE  updateCitation #-}
{-# NOINLINE  destroyCitation #-}

createCitationContributor :<|> readCitationContributor :<|> updateCitationContributor :<|> destroyCitationContributor = toClient apiCitationContributor def
{-# NOINLINE  createCitationContributor #-}
{-# NOINLINE  readCitationContributor #-}
{-# NOINLINE  updateCitationContributor #-}
{-# NOINLINE  destroyCitationContributor #-}

createCitationKeyword :<|> readCitationKeyword :<|> updateCitationKeyword :<|> destroyCitationKeyword = toClient apiCitationKeyword def
{-# NOINLINE  createCitationKeyword #-}
{-# NOINLINE  readCitationKeyword #-}
{-# NOINLINE  updateCitationKeyword #-}
{-# NOINLINE  destroyCitationKeyword #-}

createCitationProposal :<|>readCitationProposal :<|> updateCitationProposal :<|> destroyCitationProposal = toClient apiCitationProposal def
{-# NOINLINE  createCitationProposal #-}
{-# NOINLINE  readCitationProposal #-}
{-# NOINLINE  updateCitationProposal #-}
{-# NOINLINE  destroyCitationProposal #-}

createContributor :<|> readContributor :<|> updateContributor :<|> destroyContributor = toClient apiContributor def
{-# NOINLINE  createContributor #-}
{-# NOINLINE  readContributor #-}
{-# NOINLINE  updateContributor #-}
{-# NOINLINE  destroyContributor #-}

createFile :<|> readFile :<|> updateFile :<|> destroyFile = toClient apiFile def
{-# NOINLINE  createFile #-}
{-# NOINLINE  readFile #-}
{-# NOINLINE  updateFile #-}
{-# NOINLINE  destroyFile #-}

createFileKeyValue :<|> readFileKeyValue :<|> updateFileKeyValue :<|> destroyFileKeyValue = toClient apiFileKeyValue def
{-# NOINLINE  createFileKeyValue #-}
{-# NOINLINE  readFileKeyValue #-}
{-# NOINLINE  updateFileKeyValue #-}
{-# NOINLINE  destroyFileKeyValue #-}

createGroup :<|> readGroup :<|> updateGroup :<|> destroyGroup = toClient apiGroup def
{-# NOINLINE  createGroup #-}
{-# NOINLINE  readGroup #-}
{-# NOINLINE  updateGroup #-}
{-# NOINLINE  destroyGroup #-}

createInstitution :<|> readInstitution :<|> updateInstitution :<|> destroyInstitution = toClient apiInstitution def
{-# NOINLINE  createInstitution #-}
{-# NOINLINE  readInstitution #-}
{-# NOINLINE  updateInstitution #-}
{-# NOINLINE  destroyInstitution #-}

createInstitutionPerson :<|> readInstitutionPerson :<|> updateInstitutionPerson :<|> destroyInstitutionPerson = toClient apiInstitutionPerson def
{-# NOINLINE  createInstitutionPerson #-}
{-# NOINLINE  readInstitutionPerson #-}
{-# NOINLINE  updateInstitutionPerson #-}
{-# NOINLINE  destroyInstitutionPerson #-}

createInstrument :<|> readInstrument :<|> updateInstrument :<|> destroyInstrument = toClient apiInstrument def
{-# NOINLINE  createInstrument #-}
{-# NOINLINE  readInstrument #-}
{-# NOINLINE  updateInstrument #-}
{-# NOINLINE  destroyInstrument #-}

createInstrumentCustodian :<|> readInstrumentCustodian :<|> updateInstrumentCustodian :<|> destroyInstrumentCustodian = toClient apiInstrumentCustodian def
{-# NOINLINE  createInstrumentCustodian #-}
{-# NOINLINE  readInstrumentCustodian #-}
{-# NOINLINE  updateInstrumentCustodian #-}
{-# NOINLINE  destroyInstrumentCustodian #-}

createInstrumentGroup :<|> readInstrumentGroup :<|> updateInstrumentGroup :<|> destroyInstrumentGroup = toClient apiInstrumentGroup def
{-# NOINLINE  createInstrumentGroup #-}
{-# NOINLINE  readInstrumentGroup #-}
{-# NOINLINE  updateInstrumentGroup #-}
{-# NOINLINE  destroyInstrumentGroup #-}

createJournal :<|> readJournal :<|> updateJournal :<|> destroyJournal = toClient apiJournal def
{-# NOINLINE  createJournal #-}
{-# NOINLINE  readJournal #-}
{-# NOINLINE  updateJournal #-}
{-# NOINLINE  destroyJournal #-}

createKey :<|> readKey :<|> updateKey :<|> destroyKey = toClient apiKey def
{-# NOINLINE  createKey #-}
{-# NOINLINE  readKey #-}
{-# NOINLINE  updateKey #-}
{-# NOINLINE  destroyKey #-}

createKeyword :<|> readKeyword :<|> updateKeyword :<|> destroyKeyword = toClient apiKeyword def
{-# NOINLINE  createKeyword #-}
{-# NOINLINE  readKeyword #-}
{-# NOINLINE  updateKeyword #-}
{-# NOINLINE  destroyKeyword #-}

createProposal :<|> readProposal :<|> updateProposal :<|> destroyProposal = toClient apiProposal def
{-# NOINLINE  createProposal #-}
{-# NOINLINE  readProposal #-}
{-# NOINLINE  updateProposal #-}
{-# NOINLINE  destroyProposal #-}

createProposalInstrument :<|> readProposalInstrument :<|> updateProposalInstrument :<|> destroyProposalInstrument = toClient apiProposalInstrument def
{-# NOINLINE  createProposalInstrument #-}
{-# NOINLINE  readProposalInstrument #-}
{-# NOINLINE  updateProposalInstrument #-}
{-# NOINLINE  destroyProposalInstrument #-}

createProposalParticipant :<|> readProposalParticipant :<|> updateProposalParticipant :<|> destroyProposalParticipant = toClient apiProposalParticipant def
{-# NOINLINE  createProposalParticipant #-}
{-# NOINLINE  readProposalParticipant #-}
{-# NOINLINE  updateProposalParticipant #-}
{-# NOINLINE  destroyProposalParticipant #-}

createTransaction :<|> readTransaction :<|> updateTransaction :<|> destroyTransaction = toClient apiTransaction def
{-# NOINLINE  createTransaction #-}
{-# NOINLINE  readTransaction #-}
{-# NOINLINE  updateTransaction #-}
{-# NOINLINE  destroyTransaction #-}

createTransactionKeyValue :<|> readTransactionKeyValue :<|> updateTransactionKeyValue :<|> destroyTransactionKeyValue = toClient apiTransactionKeyValue def
{-# NOINLINE  createTransactionKeyValue #-}
{-# NOINLINE  readTransactionKeyValue #-}
{-# NOINLINE  updateTransactionKeyValue #-}
{-# NOINLINE  destroyTransactionKeyValue #-}

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

readUser'
  :: ()
  => UserId -- ^ _id
  -> CurlClientM (Maybe User)
readUser' userId = safeHead <$> readUser (Just userId) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing (Just 1) (Just 1)
{-# INLINE  readUser' #-}

createUserGroup :<|> readUserGroup :<|> updateUserGroup :<|> destroyUserGroup = toClient apiUserGroup def
{-# NOINLINE  createUserGroup #-}
{-# NOINLINE  readUserGroup #-}
{-# NOINLINE  updateUserGroup #-}
{-# NOINLINE  destroyUserGroup #-}

createValue :<|> readValue :<|> updateValue :<|> destroyValue = toClient apiValue def
{-# NOINLINE  createValue #-}
{-# NOINLINE  readValue #-}
{-# NOINLINE  updateValue #-}
{-# NOINLINE  destroyValue #-}

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
runLinkWith :: (ByteString -> Either String a) -> Link -> CurlRequest a
runLinkWith eitherDecode l = CurlRequest
  { _curlRequestBody = _linkRequestBody l
  , _curlRequestMethod = _linkRequestMethod l
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

instance (KnownSymbol sym, ToJSON v, HasClient sub) => HasClient (QueryParam sym v :> sub) where
  type MkClient (QueryParam sym v :> sub) = Maybe v -> MkClient sub
  toClient (Proxy :: Proxy (QueryParam sym a :> sub)) l vMaybe = toClient (Proxy :: Proxy sub) $ maybe id (addQueryParam k . Data.ByteString.Lazy.Char8.unpack . Data.Aeson.encode) vMaybe l
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
  toClient (Proxy :: Proxy (Verb method statusCode '[JSON] NoContent)) = Network.Curl.Client.fromCurlRequest . runLinkWith (const $ Right NoContent) . setRequestMethod (Proxy :: Proxy method)
  {-# INLINE  toClient #-}

instance {-# OVERLAPPABLE #-} (ReflectMethod method, KnownNat statusCode, FromJSON a) => HasClient (Verb (method :: StdMethod) (statusCode :: Nat) '[JSON] a) where
  type MkClient (Verb method statusCode '[JSON] a) = CurlClientM a
  toClient (Proxy :: Proxy (Verb method statusCode '[JSON] a)) = Network.Curl.Client.fromCurlRequest . runLinkWith Data.Aeson.eitherDecode' . setRequestMethod (Proxy :: Proxy method)
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
