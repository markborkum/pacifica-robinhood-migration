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
    createAnalyticalTool , readAnalyticalTool , readAnalyticalTool' , updateAnalyticalTool , updateAnalyticalTool' , destroyAnalyticalTool , destroyAnalyticalTool'
    -- ** AnalyticalToolProposal type
  , createAnalyticalToolProposal , readAnalyticalToolProposal , updateAnalyticalToolProposal , destroyAnalyticalToolProposal
    -- ** AnalyticalToolTransaction type
  , createAnalyticalToolTransaction , readAnalyticalToolTransaction , updateAnalyticalToolTransaction , destroyAnalyticalToolTransaction
    -- ** Citation type
  , readCitation
    -- ** CitationContributor type
  , readCitationContributor
    -- ** CitationKeyword type
  , readCitationKeyword
    -- ** CitationProposal type
  , readCitationProposal
    -- ** Contributor type
  , readContributor
    -- ** File type
  , readFile
    -- ** FileKeyValue type
  , readFileKeyValue
    -- ** Group type
  , readGroup
    -- ** Institution type
  , readInstitution
    -- ** InstitutionPerson type
  , readInstitutionPerson
    -- ** Instrument type
  , readInstrument
    -- ** InstrumentCustodian type
  , readInstrumentCustodian
    -- ** InstrumentGroup type
  , readInstrumentGroup
    -- ** Journal type
  , readJournal
    -- ** Key type
  , readKey
    -- ** Keyword type
  , readKeyword
    -- ** Proposal type
  , readProposal
    -- ** ProposalInstrument type
  , readProposalInstrument
    -- ** ProposalParticipant type
  , readProposalParticipant
    -- ** Transaction type
  , readTransaction
    -- ** TransactionKeyValue type
  , readTransactionKeyValue
    -- ** User type
  , readUser , readUser'
    -- ** UserGroup type
  , readUserGroup
    -- ** Value type
  , readValue
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
  -> Maybe Text -- ^ encoding
  -> Maybe Text -- ^ name
  -> Maybe LocalTime -- ^ created
  -> Maybe LocalTime -- ^ deleted
  -> Maybe LocalTime -- ^ updated
  -> AnalyticalTool -- ^ DATA
  -> CurlClientM NoContent
destroyAnalyticalTool
  :: ()
  => Maybe AnalyticalToolId -- ^ _id
  -> Maybe Text -- ^ encoding
  -> Maybe Text -- ^ name
  -> Maybe LocalTime -- ^ created
  -> Maybe LocalTime -- ^ deleted
  -> Maybe LocalTime -- ^ updated
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

updateAnalyticalTool'
  :: ()
  => AnalyticalToolId -- ^ _id
  -> AnalyticalTool -- ^ DATA
  -> CurlClientM NoContent
updateAnalyticalTool' analyticalToolId = updateAnalyticalTool (Just analyticalToolId) Nothing Nothing Nothing Nothing Nothing
{-# INLINE  updateAnalyticalTool' #-}

destroyAnalyticalTool'
  :: ()
  => AnalyticalToolId -- ^ _id
  -> CurlClientM NoContent
destroyAnalyticalTool' analyticalToolId = destroyAnalyticalTool (Just analyticalToolId) Nothing Nothing Nothing Nothing Nothing
{-# INLINE  destroyAnalyticalTool' #-}

createAnalyticalToolProposal :<|> readAnalyticalToolProposal :<|> updateAnalyticalToolProposal :<|> destroyAnalyticalToolProposal = toClient apiAnalyticalToolProposal def
{-# NOINLINE  createAnalyticalToolProposal #-}
{-# NOINLINE  readAnalyticalToolProposal #-}
{-# NOINLINE  updateAnalyticalToolProposal #-}
{-# NOINLINE  destroyAnalyticalToolProposal #-}

createAnalyticalToolTransaction :<|> readAnalyticalToolTransaction :<|> updateAnalyticalToolTransaction :<|> destroyAnalyticalToolTransaction = toClient apiAnalyticalToolTransaction def
{-# NOINLINE  createAnalyticalToolTransaction #-}
{-# NOINLINE  readAnalyticalToolTransaction #-}
{-# NOINLINE  updateAnalyticalToolTransaction #-}
{-# NOINLINE  destroyAnalyticalToolTransaction #-}

readCitation = toClient apiCitation def
{-# NOINLINE  readCitation #-}

readCitationContributor = toClient apiCitationContributor def
{-# NOINLINE  readCitationContributor #-}

readCitationKeyword = toClient apiCitationKeyword def
{-# NOINLINE  readCitationKeyword #-}

readCitationProposal = toClient apiCitationProposal def
{-# NOINLINE  readCitationProposal #-}

readContributor = toClient apiContributor def
{-# NOINLINE  readContributor #-}

readFile = toClient apiFile def
{-# NOINLINE  readFile #-}

readFileKeyValue = toClient apiFileKeyValue def
{-# NOINLINE  readFileKeyValue #-}

readGroup = toClient apiGroup def
{-# NOINLINE  readGroup #-}

readInstitution = toClient apiInstitution def
{-# NOINLINE  readInstitution #-}

readInstitutionPerson = toClient apiInstitutionPerson def
{-# NOINLINE  readInstitutionPerson #-}

readInstrument = toClient apiInstrument def
{-# NOINLINE  readInstrument #-}

readInstrumentCustodian = toClient apiInstrumentCustodian def
{-# NOINLINE  readInstrumentCustodian #-}

readInstrumentGroup = toClient apiInstrumentGroup def
{-# NOINLINE  readInstrumentGroup #-}

readJournal = toClient apiJournal def
{-# NOINLINE  readJournal #-}

readKey = toClient apiKey def
{-# NOINLINE  readKey #-}

readKeyword = toClient apiKeyword def
{-# NOINLINE  readKeyword #-}

readProposal = toClient apiProposal def
{-# NOINLINE  readProposal #-}

readProposalInstrument = toClient apiProposalInstrument def
{-# NOINLINE  readProposalInstrument #-}

readProposalParticipant = toClient apiProposalParticipant def
{-# NOINLINE  readProposalParticipant #-}

readTransaction = toClient apiTransaction def
{-# NOINLINE  readTransaction #-}

readTransactionKeyValue = toClient apiTransactionKeyValue def
{-# NOINLINE  readTransactionKeyValue #-}

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
  -> CurlClientM [User]
readUser = toClient apiUser def
{-# NOINLINE  readUser #-}

readUser'
  :: ()
  => UserId -- ^ _id
  -> CurlClientM (Maybe User)
readUser' userId = safeHead <$> readUser (Just userId) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
{-# INLINE  readUser' #-}

readUserGroup = toClient apiUserGroup def
{-# NOINLINE  readUserGroup #-}

readValue = toClient apiValue def
{-# NOINLINE  readValue #-}

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
