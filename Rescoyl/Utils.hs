{-# LANGUAGE OverloadedStrings #-}
module Rescoyl.Utils where

import Control.Monad (liftM)
import Data.Binary.Get (pushChunk, pushChunks)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as L
import Data.Digest.Pure.SHA (completeSha256Incremental, sha256Incremental, showDigest)
import qualified Data.Enumerator as E
import Data.List (isPrefixOf)
import Data.Version
import Snap (liftIO)
import Snap.Core
  ( getHeader, getTimeoutModifier, getsRequest
  , runRequestBody
  , MonadSnap(..))
import Snap.Snaplet (Handler)
import System.Directory (renameFile)
import System.FilePath ((<.>))
import System.IO (hClose)
import System.IO.Temp (openTempFile)
import Text.ParserCombinators.ReadP (readP_to_S)

import Rescoyl.Types

----------------------------------------------------------------------
-- Extract a docker client's version.
----------------------------------------------------------------------

-- | This docker client version is special in that it is the beginning of a
-- specific way to compute a layer checksum.
zero_ten :: Version
zero_ten = Version [0, 10, 0] []

-- | Return the Docker client version in our Snap monad.
getClientVersion :: Handler App App (Maybe Version)
getClientVersion = do
  mua <- getsRequest (getHeader "User-Agent")
  case mua of
    Nothing -> return Nothing
    Just ua -> return $ toVersion ua

toVersion :: ByteString -> Maybe Version
toVersion ua =
  case filter (isPrefixOf "docker/") $ words $ BC.unpack ua of
    [] -> Nothing
    docker:_ -> case (readP_to_S parseVersion $ drop 7 docker) of
      (v,_):_ -> Just v { versionTags = [] }
      _ -> Nothing

----------------------------------------------------------------------
-- Write an image layer to disk.
----------------------------------------------------------------------

-- | Write the request body (expected to be an image layer) to the give path.
-- Return its checksum and size. The checksum includes the raw JSON string
-- (which is provided in argument).
saveImageLayerToFile :: MonadSnap m => ByteString -> FilePath -> m (ByteString, Int)
saveImageLayerToFile json path = do
  -- Note that we need the json string exactly as it was sent by the client.
  -- It cannot be the result of parse/render unless it gives the exact same
  -- result.
  let decoder = pushChunk sha256Incremental $ json `B.append` "\n"
      n = B.length json + 1
  bump <- liftM ($ max 5) getTimeoutModifier
  -- The temporary file must be located at the same place than the final file.
  -- This is necessary because the `renameFile` operation won't work if the
  -- data store is a Docker bind-mount and the temporary location isn't.
  (fn, h) <- liftIO $ openTempFile "/" $ path <.> "temp"
  r <- runRequestBody $ iterHandle' decoder n bump h
  liftIO $ do
    hClose h
    renameFile fn path
  return r

--iterHandle' :: MonadIO m => IO.Handle -> Iteratee ByteString m ()
iterHandle' decoder n' bump h = E.continue $ step decoder n' where
  step dec n E.EOF =
    E.yield (BC.pack . showDigest $ completeSha256Incremental dec n, n) E.EOF
  step dec n (E.Chunks []) = E.continue $ step dec n
  step dec n (E.Chunks bytes) = do
    E.tryIO (mapM_ (B.hPut h) bytes)
    _ <- E.tryIO bump
    let bytes' = L.fromChunks bytes
    E.continue $ step (pushChunks dec bytes') (n + fromIntegral (L.length bytes'))
    -- TODO I wonder if the fromIntegral to use Int instead of Int64
    -- means long input would get a wrong SHA256 sum.
