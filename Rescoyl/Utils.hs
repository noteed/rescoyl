{-# LANGUAGE OverloadedStrings #-}
module Rescoyl.Utils where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import Data.List (isPrefixOf)
import Data.Version
import Snap.Core (getHeader, getsRequest)
import Snap.Snaplet (Handler)
import Text.ParserCombinators.ReadP (readP_to_S)

import Rescoyl.Types

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
