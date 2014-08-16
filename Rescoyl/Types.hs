{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
-- | This module, in addition to basic data types, also define two backend
-- interfaces. The rescoyl package comes with backends using the file-system.
-- That backends are in `Rescoyl.Simple`.
module Rescoyl.Types where

import Control.Applicative ((<$>), (<*>))
import Control.Lens (makeLenses)
import Control.Monad (mzero)
import Data.Aeson
import Data.List (foldl')
import qualified Data.Map as M
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as L
import Snap.Snaplet (Handler, Snaplet)
import Snap.Snaplet.Session (SessionManager)

----------------------------------------------------------------------
-- Backends
----------------------------------------------------------------------

-- | This interface is used for authentication.
data UserBackend = UserBackend
  { isAuthorized :: Maybe (ByteString, ByteString) -> IO (Maybe ByteString)
  -- ^ Given a username and password, this returns the user's namespace.
  }

-- | This interface is used to store the repositories and images.
-- TODO The Handler App App () types must be replaced by non-HTTP-related code.
-- This is currently required to e.g. serve an image layer. This can be changed
-- when Snap is written using the `io-streams` library.
data RegistryBackend = RegistryBackend
  { imageAncestry :: ByteString -> ByteString -> IO (Maybe Value)
  , imageJson :: ByteString -> ByteString -> IO GetJson
  , imageLayer :: ByteString -> ByteString -> Handler App App ()
  , saveImageJson :: ByteString -> ByteString
      -> ImageDescription -> L.ByteString -> Handler App App ()
  , saveImageLayer :: ByteString -> ByteString -> Handler App App ()
  , saveImageChecksum :: ByteString -> ByteString -> ByteString -> IO PutChecksum
  , saveImageChecksumOld :: ByteString -> ByteString -> ByteString -> IO PutChecksum
  -- ^ For older checksum header.
  , saveRepository :: ByteString -> ByteString -> [ImageInfo] -> IO ()
  , readTags :: ByteString -> ByteString -> IO Value
  , saveTag :: ByteString -> ByteString -> ByteString -> L.ByteString -> IO ()
  , readImageIndex :: ByteString -> ByteString -> IO (Maybe [ImageInfo])
  , saveImageIndex :: ByteString -> ByteString -> [ImageInfo] -> Handler App App ()
  }

data GetJson =
    NoSuchImage
  | UploadInProgress
  | SizeAndJson Int Value
  | ErrorDecodingJson

data PutChecksum =
    ChecksumNoSuchImage
  | ChecksumSaved
  | ChecksumMismatch
  | ChecksumAlreadySaved

----------------------------------------------------------------------
-- JSON
----------------------------------------------------------------------

-- | Represent image data in a `images` index file.
data ImageInfo = ImageInfo
  { imageInfoId :: String
  , imageInfoChecksum :: Maybe String
  , imageInfoTag :: Maybe String
  }
  deriving Show

instance FromJSON ImageInfo where
  parseJSON (Object v) = ImageInfo <$>
    v .: "id" <*>
    v .:? "checksum" <*>
    v .:? "Tag"
  parseJSON _ = mzero

instance ToJSON ImageInfo where
  toJSON ImageInfo{..} = object $
    [ "id" .= imageInfoId
    ]
    ++ maybe [] ((: []) . ("Tag" .=)) imageInfoTag
    ++ maybe [] ((: []) . ("checksum" .=)) imageInfoChecksum

combineImageInfo :: [ImageInfo] -> [ImageInfo] -> [ImageInfo]
combineImageInfo a b = M.elems $ foldl' combine M.empty $ a ++ b
  where
  combine m i =
    let mchecksum = do
          i' <- M.lookup (imageInfoId i) m
          imageInfoChecksum i'
    in maybe (M.insert (imageInfoId i) i m) (const m) mchecksum

-- | Represent image meta data in a `json` file.
data ImageDescription = ImageDescription
  { imageDescriptionId :: String
  , imageDescriptionParent :: Maybe String
  }
  deriving Show

instance FromJSON ImageDescription where
  parseJSON (Object v) = ImageDescription <$>
    v .: "id" <*>
    v .:? "parent"
  parseJSON _ = mzero

instance ToJSON ImageDescription where
  toJSON ImageDescription{..} = object $
    maybe [] ((: []) . ("parent" .=)) imageDescriptionParent

-- | Represent data sent by `docker login`.
-- This looks like:
--
--   Object fromList [("auth",String ""),("password",String "d"),
--     ("email",String "e@e.com"),("username",String "c")]
data Registration = Registration
  { registrationAuth :: String
  , registrationUsername :: String
  , registrationPassword :: String
  , registrationEmail :: String
  }
  deriving Show

instance FromJSON Registration where
  parseJSON (Object v) = Registration <$>
    v .: "auth" <*>
    v .: "username" <*>
    v .: "password" <*>
    v .: "email"
  parseJSON _ = mzero

----------------------------------------------------------------------
-- Snap Application
----------------------------------------------------------------------

data App = App
  { _sess :: Snaplet SessionManager
  , _users :: UserBackend
  , _registry :: RegistryBackend
  }

makeLenses ''App
