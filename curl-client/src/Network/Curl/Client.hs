{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module:      Network.Curl.Client
-- Copyright:   (c) 2017 Pacific Northwest National Laboratory
-- License:     LGPL
-- Maintainer:  Mark Borkum <mark.borkum@pnnl.gov>
-- Stability:   experimental
-- Portability: portable
--
-- This module exports Haskell types and functions for a cURL-based client.
--
module Network.Curl.Client
  ( -- * cURL client monad type
    CurlClientM
  , runCurlClientM
    -- * cURL types
  , CurlClientEnv(..)
  , CurlClientErr(..)
    -- * cURL request type
  , CurlRequest(..)
  , fromCurlRequest
  ) where

import           Control.Monad.Error.Class (MonadError(throwError))
import           Control.Monad.IO.Class (MonadIO(liftIO))
import           Control.Monad.Reader.Class (MonadReader(), asks)
import           Control.Monad.Trans.Except (ExceptT, runExceptT)
import           Control.Monad.Trans.Reader (ReaderT, runReaderT)
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy
import           Network.HTTP.Types (StdMethod)
import           Network.URL (URL, URLType)
import qualified Network.URL
import           System.Exit (ExitCode(..))
import qualified System.IO
import           System.Process (CmdSpec(..), StdStream(CreatePipe, NoStream))
import qualified System.Process
import qualified Text.Printf

-- | The cURL client monad.
--
newtype CurlClientM a = CurlClientM { getCurlClientM :: ReaderT CurlClientEnv (ExceptT CurlClientErr IO) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadError CurlClientErr, MonadReader CurlClientEnv)

runCurlClientM :: CurlClientM a -> CurlClientEnv -> IO (Either CurlClientErr a)
runCurlClientM = (runExceptT .) . runReaderT . getCurlClientM
{-# INLINE  runCurlClientM #-}

-- | The environment for the cURL client monad.
--
data CurlClientEnv = CurlClientEnv CmdSpec URLType

-- | An error within the context of the cURL client monad.
--
data CurlClientErr
  = CurlClientDecodeFailed String
  | CurlClientNonZeroExitCode ByteString Int
  deriving (Eq, Ord, Read, Show)

-- | A cURL request.
--
data CurlRequest a = CurlRequest
  { _curlRequestBody :: Maybe ByteString
  , _curlRequestMethod :: Maybe StdMethod
  , _curlRequestMkURL :: URLType -> URL
  , _curlRequestDecode :: ByteString -> Either String a
  }

-- | Construct a new cURL client whose monadic computation yields the result of sending the given request and decoding the response.
--
fromCurlRequest :: CurlRequest a -> CurlClientM a
fromCurlRequest (CurlRequest { _curlRequestBody = std_in_Maybe , _curlRequestMethod = stdMethodMaybe , _curlRequestMkURL = mkURL , _curlRequestDecode = eitherDecode }) = CurlClientM $ do
  cp <- asks $ \(CurlClientEnv spec0 url_type0) -> case maybe id setMethod stdMethodMaybe $ setURL (mkURL url_type0) $ spec0 of
    ShellCommand cmd_args -> System.Process.shell cmd_args
    RawCommand cmd args -> System.Process.proc cmd args
  (h_std_in_Maybe, Just h_std_out, Just h_std_err, h_proc) <- liftIO $ System.Process.createProcess cp
    { System.Process.std_in = maybe NoStream (const CreatePipe) std_in_Maybe
    , System.Process.std_out = CreatePipe
    , System.Process.std_err = CreatePipe
    }
  maybe (return ()) (uncurry $ \h_std_in std_in -> liftIO $ Data.ByteString.Lazy.hPutStr h_std_in std_in >> System.IO.hClose h_std_in) $ pure (,) <*> h_std_in_Maybe <*> std_in_Maybe
  exitCode <- liftIO $ System.Process.waitForProcess h_proc
  std_out <- liftIO $ Data.ByteString.Lazy.hGetContents h_std_out
  std_err <- liftIO $ Data.ByteString.Lazy.hGetContents h_std_err
  case exitCode of
    ExitSuccess -> case eitherDecode std_out of
      Left msg -> throwError $ CurlClientDecodeFailed msg
      Right x -> return x
    ExitFailure n -> throwError $ CurlClientNonZeroExitCode std_err n
{-# INLINE  fromCurlRequest #-}

-- | Set the URL.
--
setURL :: URL -> CmdSpec -> CmdSpec
setURL url spec =
  let
    urlString :: String
    urlString = Network.URL.exportURL url
  in case spec of
    ShellCommand cmd_args -> ShellCommand $ Text.Printf.printf "%s '%s'" cmd_args urlString
    RawCommand cmd args -> RawCommand cmd $ snoc urlString args
{-# INLINE  setURL #-}

-- | Set the HTTP method.
--
setMethod :: StdMethod -> CmdSpec -> CmdSpec
setMethod stdMethod spec =
  let
    stdMethodString :: String
    stdMethodString = show stdMethod
  in case spec of
    ShellCommand cmd_args -> ShellCommand $ Text.Printf.printf "%s -X %s" cmd_args stdMethodString
    RawCommand cmd args -> RawCommand cmd $ "-X" : stdMethodString : args
{-# INLINE  setMethod #-}

-- | Append an element to a list.
--
snoc :: a -> [a] -> [a]
snoc x xs = xs ++ [x]
{-# INLINE  snoc #-}
