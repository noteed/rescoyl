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
import Data.Text (Text)
import Snap.Snaplet (Handler, Snaplet)
import Snap.Snaplet.Session (SessionManager)

----------------------------------------------------------------------
-- Backends
----------------------------------------------------------------------

-- | This interface is used for authentication.
data UserBackend = UserBackend
  { isAuthorized :: Maybe (Text, Text) -> IO (Maybe Text)
  -- ^ Given a username and password, this returns the user's namespace.
  }

-- | This interface is used to store the repositories and images.
-- TODO The Handler App App () types must be replaced by non-HTTP-related code.
-- This is currently required to e.g. serve an image layer. This can be changed
-- when Snap is written using the `io-streams` library.
--
-- Given a unique (namespace, image ID) pair, it is guaranteed by the protocol
-- handlers to call `saveImageJson`, `saveImageLayer`, and `saveImageChecksum`
-- in that order (when the previous function has been succesfull). The success
-- of these functions must be visible in the result of `loadImage`.
-- Calls to saveImageJson can be repeated.
--
-- TODO To be really correct, a backend should provide a withTransaction
-- function, allowing to chain `loadImage` and, say, `saveImageLayer`
-- atomically.
data RegistryBackend = RegistryBackend
  { loadImage :: Text -> Text -> IO GetImage
  , saveImageJson :: Text -> Text
      -> ImageDescription -> L.ByteString -> IO ()
  , saveImageLayer :: Text -> Text -> Handler App App ()
  , saveImageChecksum :: Text -> Text -> ByteString -> IO ()
  , saveImageChecksumOld :: Text -> Text -> ByteString -> IO ()
  -- ^ For older checksum header.
  , saveRepository :: ByteString -> ByteString -> [ImageInfo] -> IO ()
  , readTags :: ByteString -> ByteString -> IO Value
  , saveTag :: ByteString -> ByteString -> ByteString -> L.ByteString -> IO ()
  , readImageIndex :: ByteString -> ByteString -> IO (Maybe [ImageInfo])
  , saveImageIndex :: ByteString -> ByteString -> [ImageInfo] -> Handler App App ()
  }

-- | The minimal data that a server has about an image is its JSON meta-data.
-- Without that meta-data, the image doesn't exist.
data GetImage =
    ImageJson Value Value
    -- ^ JSON meta-data, and ancestry data.
  | ImageLayer Value Value Layer
    -- ^ Same as ImageJson, but adds a layer.
  | Image Value Value Layer ByteString
    -- ^ Same as ImageLayer, but adds a client-provided checksum.
  | ImageErrorDecodingJson
  | ImageDoesntExist

data Layer = Layer FilePath Int ByteString
  -- ^ Currently the layer is assumed to be on disk. A variant type
  -- `FilePath | InputStream ByteString` will be used in the future.
  -- In addition to the path on disk, the layer size and its checksum are
  -- given. A layer value shouldn't be constructed if the data is not fully
  -- available on disk (e.g. if the layer is currently being uploaded; since
  -- the size and checksum are required, it is difficult to not obey that
  -- rule).

data GetJson =
    NoSuchImage
  | UploadInProgress
  | SizeAndJson Int Value
  | ErrorDecodingJson

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
