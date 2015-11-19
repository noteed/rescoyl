{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-} -- For the specialized setXXX headers.
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | This module defines the Snap routes and handlers. It is supposed to deal
-- with the HTTP nature of the Docker registry protocol then delegate to the
-- backends.
-- See `appInit` in the main script to see how to use your own backends.
module Rescoyl.Handlers where

import Control.Applicative ((<|>), (<$>))
import Control.Exception (SomeException)
import Control.Monad (when)
import Control.Monad.CatchIO (catch)
import Control.Monad.State (gets)
import Control.Monad.Trans (liftIO)
import Data.Aeson (decode, encode, object, (.=))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as L
import Data.List (intersperse)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.UUID as UUID
import Data.Version (showVersion)
import Snap.Core
  ( emptyResponse, finishWith, getHeader, getParam, getResponse, getsRequest
  , ifTop, logError, method
  , modifyResponse, pass, putResponse, readRequestBody
  , setContentType, setHeader, setResponseStatus, writeLBS, writeText
  , setContentLength
  , Method(..))
import Snap.Snaplet (Handler)
import Snap.Snaplet.Session (withSession)
import Snap.Util.FileServe (serveFile)
import System.Directory (doesFileExist)

import Paths_rescoyl (version)

import Rescoyl.Simple (notFound)
import Rescoyl.Types
import Rescoyl.Utils (zero_ten, getClientVersion)

routes :: [String] -> [(ByteString, Handler App App ())]
routes endpoints =
  -- Both / and /v1 are provided for humans. In particular, /v1 is reported
  -- by `docker login` as containing account activation instructions.
  [ ("/", ifTop $ method GET indexPage)
  , ("/v1", ifTop $ method GET v1Page)

  , ("/v1/_ping", ifTop $ method GET ping)

  , ("/v1/images/:image/ancestry", ifTop $ method GET getImageAncestry)
  , ("/v1/images/:image/json", ifTop $
    method GET getImageJson <|> method PUT putImageJson)
  -- TODO Also handle HEAD requests for the layer.
  , ("/v1/images/:image/layer", ifTop $
    method GET getImageLayer <|> method PUT putImageLayer)
  , ("/v1/images/:image/checksum", ifTop $ method PUT putImageChecksum)

  -- TODO handle :repo in addition to :repo/
  , ("/v1/repositories/:repo/", ifTop $ method PUT $ putRepository endpoints)
  , ("/v1/repositories/:namespace/:repo/", ifTop $ method PUT $ do
    -- Make sure to not shadow "/v1/repositories/:repo/images" below.
    Just repo <- getParam "repo"
    if repo == "images" then pass else putRepository endpoints)

  , ("/v1/repositories/:repo/tags/:tag", ifTop $ method PUT putTag)
  , ("/v1/repositories/:namespace/:repo/tags/:tag", ifTop $
    method GET getTag <|> method PUT putTag)

  , ("/v1/repositories/:repo/images", ifTop $
    method GET (getImageIndex endpoints) <|> method PUT putImageIndex)
  , ("/v1/repositories/:namespace/:repo/images", ifTop $
    method GET (getImageIndex endpoints) <|> method PUT putImageIndex)

  , ("/v1/repositories/:namespace/:repo/tags", ifTop $ method GET getTags)

  , ("/v1/users", ifTop $
    method GET getUsers <|>
    method POST postUser <|>
    method PUT (error "TODO PUT /v1/users"))

  , ("/v2", ifTop (method GET v2Page))
  , ("/v2/:namespace/:repo/blobs/uploads", ifTop (method POST v2StartUpload))
  , ("/v2/:namespace/:repo/blobs/uploads/:uuid", ifTop
      (method PATCH v2UploadChunk <|> method PUT v2CompleteUpload))
  , ("/v2/:namespace/:repo/blobs/:digest", ifTop (method HEAD v2HeadLayer))

  , ("/500", error "Intentional 500.")
  ]

catch500 :: Handler App App a -> Handler App App ()
catch500 m = (m >> return ()) `catch` \(e::SomeException) -> do
  putResponse $ setContentType "text/html" $
    setResponseStatus 500 "Internal Server Error" emptyResponse
  writeText "The Rescoyl Docker registry exploded. Exception:\n\n"
  writeText $ T.pack $ show e

  logError $ B.concat [ "caught exception: ", B.pack $ show e ]

indexPage :: Handler App App ()
indexPage = do
  writeText . T.pack $ "The Rescoyl Docker registry " ++ showVersion version ++ "."

v1Page :: Handler App App ()
v1Page = do
  writeText . T.pack $ "The Rescoyl Docker registry " ++ showVersion version ++ "."

ping :: Handler App App ()
ping = withSession sess $ do
  modifyResponse $ setHeader "X-Docker-Registry-Version" "0.7.0"
  writeText "true"

getImageAncestry :: Handler App App ()
getImageAncestry = do
  Just image <- getParam "image"
  namespace <- validateGetImage image
  reg <- gets _registry
  mi <- liftIO $ loadImage reg namespace (T.decodeUtf8 image)
  case mi of
    ImageJson _ ancestry -> do
      modifyResponse $ setContentType "application/json"
      writeLBS $ encode ancestry
    ImageLayer _ ancestry _ -> do
      modifyResponse $ setContentType "application/json"
      writeLBS $ encode ancestry
    Image _ ancestry _ _ -> do
      modifyResponse $ setContentType "application/json"
      writeLBS $ encode ancestry
    ImageErrorDecodingJson -> modifyResponse $
      setResponseStatus 500 "Error decoding already saved JSON."
    ImageDoesntExist -> notFound

getImageJson :: Handler App App ()
getImageJson = do
  Just image <- getParam "image"
  namespace <- validateGetImage image
  reg <- gets _registry

  mi <- liftIO $ loadImage reg namespace (T.decodeUtf8 image)
  case mi of
    ImageJson _ _ -> modifyResponse $
      setResponseStatus 400 "Image upload is in progress."
    ImageLayer _ _ _ -> modifyResponse $
      setResponseStatus 400 "Image upload is in progress."
    Image json _ (Layer _ size _) _ -> do
      modifyResponse $ setHeader "X-Docker-Size" $ B.pack $ show size
      modifyResponse $ setContentType "application/json"
      writeLBS $ encode json
    ImageErrorDecodingJson -> modifyResponse $
      setResponseStatus 500 "Error decoding already saved JSON."
    ImageDoesntExist -> notFound

getImageLayer :: Handler App App ()
getImageLayer = do
  Just image <- getParam "image"
  namespace <- validateGetImage image
  reg <- gets _registry
  mi <- liftIO $ loadImage reg namespace (T.decodeUtf8 image)
  case mi of
    ImageJson _ _ -> modifyResponse $
      setResponseStatus 400 "Image upload is in progress."
    ImageLayer _ _ _ -> modifyResponse $
      setResponseStatus 400 "Image upload is in progress."
    Image _ _ (Layer path _ _) _ -> serveFile' path
    ImageErrorDecodingJson -> modifyResponse $
      setResponseStatus 500 "Error decoding already saved JSON."
    ImageDoesntExist -> notFound

serveFile' :: FilePath -> Handler App App ()
serveFile' path = do
  e <- liftIO $ doesFileExist path
  if e
    then serveFile path
    else modifyResponse $ setResponseStatus 404 "No Found"

putImageJson :: Handler App App ()
putImageJson = do
  namespace <- validatePutImage
  Just image <- getParam "image"
  reg <- gets _registry
  body <- readRequestBody 65536 -- TODO correct size.
  case decode body of
    Nothing -> modifyResponse $
      setResponseStatus 400 "Error decoding JSON."
    Just desc | imageDescriptionId desc /= B.unpack image ->
      modifyResponse $ setResponseStatus 400 "Invalid image ID in JSON."
    Just desc -> do
      mi <- liftIO $ loadImage reg namespace (T.decodeUtf8 image)
      case mi of
        ImageJson _ _ -> do
          liftIO $ saveImageJson reg namespace (T.decodeUtf8 image) desc body
          writeText "true"
        ImageLayer _ _ _ -> do
          liftIO $ saveImageJson reg namespace (T.decodeUtf8 image) desc body
          writeText "true"
        Image _ _ _ _ -> modifyResponse $
          setResponseStatus 409 "Image already exists."
        ImageErrorDecodingJson -> modifyResponse $
          setResponseStatus 500 "Error decoding already saved JSON."
        ImageDoesntExist -> do
          liftIO $ saveImageJson reg namespace (T.decodeUtf8 image) desc body
          writeText "true"

putImageLayer :: Handler App App ()
putImageLayer = do
  namespace <- validatePutImage
  Just image <- getParam "image"
  reg <- gets _registry
  mi <- liftIO $ loadImage reg namespace (T.decodeUtf8 image)
  case mi of
    ImageJson _ _ -> do
      saveImageLayer reg namespace (T.decodeUtf8 image)
      writeText "true"
    ImageLayer _ _ _ -> do
      saveImageLayer reg namespace (T.decodeUtf8 image)
      writeText "true"
    Image _ _ _ _ -> modifyResponse $
      setResponseStatus 409 "Image already exists."
    ImageErrorDecodingJson -> modifyResponse $
      setResponseStatus 500 "Error decoding already saved JSON."
    ImageDoesntExist -> do
      modifyResponse $ setResponseStatus 404 "Image doesn't exist."

putImageChecksum :: Handler App App ()
putImageChecksum = do
  namespace <- validatePutImage
  Just image <- getParam "image"
  reg <- gets _registry

  mi <- liftIO $ loadImage reg namespace (T.decodeUtf8 image)
  case mi of
    ImageJson _ _ -> do
      modifyResponse $ setResponseStatus 404 "Image doesn't exist."
    ImageLayer _ _ (Layer _ _ computed) -> do
      go computed reg namespace (T.decodeUtf8 image)
    Image _ _ _ _ -> do
      modifyResponse $ setResponseStatus 409 "Checksum already saved."
      modifyResponse $ setContentType "application/json"
      writeText "{\"error\":\"Checksum already saved\"}"
    ImageErrorDecodingJson -> modifyResponse $
      setResponseStatus 500 "Error decoding already saved JSON."
    ImageDoesntExist -> notFound

  where
  go computed reg namespace image = do
    mchecksum <- getsRequest $ getHeader "X-Docker-Checksum-Payload"
    case mchecksum of
      Just checksum -> do
        mclient <- getClientVersion
        -- For older Docker clients. (TODO Should get rid of it.)
        -- Don't compare uploaded checksum against computed one.
        case mclient of
          Nothing -> liftIO $ saveImageChecksumOld reg namespace image checksum
          Just client ->
            if client < zero_ten
            then liftIO $ saveImageChecksumOld reg namespace image checksum
            else
              if checksum == computed
              then liftIO $ saveImageChecksum reg namespace image checksum
              else do
                modifyResponse $ setResponseStatus 400 "Checksum mismatch."
                modifyResponse $ setContentType "application/json"
                writeText "{\"error\":\"Checksum mismatch\"}"

      Nothing -> do
        -- For older Docker clients. TODO Should get rid of it.
        -- Don't compare uploaded checksum against computed one.
        mchecksum' <- getsRequest $ getHeader "X-Docker-Checksum"
        case mchecksum' of
          Just checksum -> do
            liftIO $ saveImageChecksumOld reg namespace image checksum
          Nothing -> do
            modifyResponse $ setResponseStatus 400 "Error reading checksum."
            modifyResponse $ setContentType "application/json"
            writeText "{\"error\":\"Error reading checksum\"}"

-- | Creates the image index STATIC/v1/repositories/:namespace/:repo/images
putRepository :: [String] -> Handler App App ()
putRepository endpoints = do
  Just namespace <- getParam "namespace"
  Just repo <- getParam "repo"
  validatePutRepository namespace repo
  reg <- gets _registry

  -- It seems the docker client requests a token even when it will only use
  -- Basic auth in the next queries.
  let token = "Token signature=UNUSED, repository=\"dummy\", access=write"
  modifyResponse $ setContentType "application/json"
  addEndpointsHeader endpoints
  modifyResponse $ setHeader "X-Docker-Token" token
  modifyResponse $ setHeader "WWW-Authenticate" token
  body <- readRequestBody 65536 -- TODO Correct size, validate content.
  case decode body of
    Nothing -> modifyResponse $
      setResponseStatus 400 "Error decoding JSON."
    Just images -> liftIO $ saveRepository reg namespace repo images
  writeText "\"\""

getTag :: Handler App App ()
getTag = do
  Just namespace <- getParam "namespace"
  Just repo <- getParam "repo"
  Just tag <- getParam "tag"
  reg <- gets _registry
  dict <- liftIO $ readTags reg namespace repo
  case lookup (T.decodeUtf8 tag) dict of
    Nothing -> modifyResponse $ setResponseStatus 404 "No Found"
    Just hash -> do
      modifyResponse $ setContentType "application/json"
      writeLBS $ encode hash

putTag :: Handler App App ()
putTag = do
  Just namespace <- getParam "namespace"
  Just repo <- getParam "repo"
  Just tag <- getParam "tag"
  validatePutRepository namespace repo
  reg <- gets _registry
  body <- readRequestBody 4096 -- TODO correct size
  let value = L.tail $ L.init body -- TODO may explode
  liftIO $ saveTag reg namespace repo tag value

getTags :: Handler App App ()
getTags = do
  Just namespace <- getParam "namespace"
  Just repo <- getParam "repo"
  reg <- gets _registry
  dict <- liftIO $ readTags reg namespace repo
  modifyResponse $ setContentType "application/json"
  writeLBS $ encode $ object $ map (uncurry (.=)) dict

getImageIndex :: [String] -> Handler App App ()
getImageIndex endpoints = do
  Just namespace <- getParam "namespace"
  Just repo <- getParam "repo"
  reg <- gets _registry
  modifyResponse $ setContentType "application/json"
  addEndpointsHeader endpoints
  mi <- liftIO $ readImageIndex reg namespace repo
  case mi of
    Nothing -> modifyResponse $
      setResponseStatus 500 "Error reading images index."
    Just i -> writeLBS $ encode i

putImageIndex :: Handler App App ()
putImageIndex = do
  Just namespace <- getParam "namespace"
  Just repo <- getParam "repo"
  validatePutRepository namespace repo
  reg <- gets _registry
  body <- readRequestBody 65536 -- TODO correct size
  case decode body of
    Nothing -> modifyResponse $
      setResponseStatus 400 "Error decoding JSON."
    Just i -> saveImageIndex reg namespace repo i

addEndpointsHeader :: [String] -> Handler App App ()
addEndpointsHeader =
  modifyResponse . setHeader "X-Docker-Endpoints"
  . B.pack . concat . intersperse ", "

getUsers :: Handler App App ()
getUsers = do
  _ <- validatePutImage
  writeLBS "OK"

-- | Used by `docker login`.
postUser :: Handler App App ()
postUser = do
  us <- gets _users
  body <- readRequestBody 4096 -- TODO correct size.
  case decode body of
    Nothing -> modifyResponse $
      setResponseStatus 400 "Error decoding JSON."
    Just reg -> do
      m <- liftIO $ isAuthorized us
        (Just (T.pack $ registrationUsername reg,
        T.pack $ registrationPassword reg))
      case m of
        Just _ -> modifyResponse $ setResponseStatus 201 "User created"
        Nothing -> modifyResponse $ setResponseStatus 401 "Unauthorized"

----------------------------------------------------------------------
-- Authorization
----------------------------------------------------------------------

-- | Check login, password, and write access rights to the namespace.
-- TODO Access rights.
validatePutRepository :: ByteString -> ByteString -> Handler App App ()
validatePutRepository namespace repo = do
  mauthorization <- getsRequest $ getHeader "Authorization"
  validatePutRepository' mauthorization namespace repo

validatePutRepository' :: Maybe ByteString -> ByteString -> ByteString -> Handler App App ()
validatePutRepository' mauthorization namespace repo = do
  us <- gets _users
  authorized <- liftIO $ isAuthorized us (mauthorization >>= unhashBasic)
  when (authorized /= Just (T.decodeUtf8 namespace)) $ do
    modifyResponse $ setResponseStatus 401 "Unauthorized"
    r <- getResponse
    finishWith r

-- | Check login, and password.
validatePutImage :: Handler App App Text
validatePutImage = do
  mauthorization <- getsRequest $ getHeader "Authorization"
  validatePutImage' mauthorization

validatePutImage' :: Maybe ByteString -> Handler App App Text
validatePutImage' mauthorization = do
  us <- gets _users
  mauthorized <- liftIO $ isAuthorized us (mauthorization >>= unhashBasic)
  mnamespace <- liftIO $ isAllowedToWriteNamespace us mauthorized
  case mnamespace of
    Nothing -> do
      modifyResponse $ setResponseStatus 401 "Unauthorized"
      r <- getResponse
      finishWith r
    Just namespace -> return namespace

-- | Check login, and password, returns a namespace from which
-- the image can be read.
validateGetImage :: ByteString -> Handler App App Text
validateGetImage image = do
  mauthorization <- getsRequest $ getHeader "Authorization"
  validateGetImage' image mauthorization

validateGetImage' :: ByteString -> Maybe ByteString -> Handler App App Text
validateGetImage' image mauthorization = do
  us <- gets _users
  mauthorized <- liftIO $ isAuthorized us (mauthorization >>= unhashBasic)
  mnamespace <- liftIO $ isAllowedToReadImage us mauthorized (T.decodeUtf8 image)
  case mnamespace of
    Nothing -> do
      modifyResponse $ setResponseStatus 401 "Unauthorized"
      r <- getResponse
      finishWith r
    Just namespace -> return namespace

hashBasic :: ByteString -> ByteString -> ByteString
hashBasic login password = ("Basic " `B.append`) . Base64.encode $
  B.concat [login, ":", password]

unhashBasic :: ByteString -> Maybe (Text, Text)
unhashBasic h =
  case Base64.decode . B.dropWhile (== ' ') . B.drop 6 $ h of
    Right x | [login, password] <- B.split ':' x -> Just (T.decodeUtf8 login, T.decodeUtf8 password)
    _ -> Nothing

----------------------------------------------------------------------
-- Protocol 2
----------------------------------------------------------------------

v2Page :: Handler App App ()
v2Page = do
  -- The official registry also sets charset=utf-8, which seems out
  -- of spec for application/json.
  modifyResponse (setContentType "application/json")
  modifyResponse setApiVersion
  writeLBS (encode (object []))

-- | TODO If a "digest" parameter is provided, this is a "direct" upload,
-- without a get-uuid and complete-uuid process.
v2StartUpload :: Handler App App ()
v2StartUpload = do
  Just namespace <- getParam "namespace"
  Just _ <- getParam "repo"
  reg2 <- gets _registry2
  (size, uuid) <- v2StartImageLayer reg2 (T.decodeUtf8 namespace)

  -- Specs says that Location and UUID are actually opaque strings.
  -- The official registry also sets a ?_state= parameter.
  modifyResponse (setResponseStatus 202 "Accepted")
  modifyResponse (setContentType "text/plain; charset=utf-8")
  modifyResponse (setHeader "Location"
    (B.append "/v2/quux/bar/blobs/uploads/" (T.encodeUtf8 uuid)))
  modifyResponse setApiVersion
  modifyResponse (setHeader "Docker-Upload-Uuid" (T.encodeUtf8 uuid))
  modifyResponse (setRange 0 size)

v2UploadChunk :: Handler App App ()
v2UploadChunk = do
  Just namespace <- getParam "namespace"
  Just _ <- getParam "repo"
  Just uuid <- (>>= UUID.fromASCIIBytes) <$> getParam "uuid"
  reg2 <- gets _registry2
  size <- v2ContinueImageLayer reg2 (T.decodeUtf8 namespace) (UUID.toText uuid)

  modifyResponse (setResponseStatus 202 "Accepted")
  modifyResponse (setContentType "text/plain; charset=utf-8")
  modifyResponse (setHeader "Location"
    (B.append "/v2/quux/bar/blobs/uploads/" (UUID.toASCIIBytes uuid)))
  modifyResponse setApiVersion
  modifyResponse (setHeader "Docker-Upload-Uuid" (UUID.toASCIIBytes uuid))
  modifyResponse (setRange 0 size)

v2CompleteUpload :: Handler App App ()
v2CompleteUpload = do
  Just namespace <- getParam "namespace"
  Just _ <- getParam "repo"
  Just uuid <- (>>= UUID.fromASCIIBytes) <$> getParam "uuid"
  reg2 <- gets _registry2
  (digest, size) <- v2CompleteImageLayer reg2 (T.decodeUtf8 namespace) (UUID.toText uuid)
  let digest' = B.concat ["sha256:", digest]

  modifyResponse (setResponseStatus 201 "Created")
  modifyResponse (setContentType "text/plain; charset=utf-8")
  modifyResponse setApiVersion
  modifyResponse (setHeader "Docker-Content-Digest" digest')
  modifyResponse (setHeader "Location"
    (B.concat ["/v2/quux/bar/blobs/", digest']))

v2HeadLayer :: Handler App App ()
v2HeadLayer = do
  Just namespace <- getParam "namespace"
  Just _ <- getParam "repo"
  Just digest <- (B.drop (B.length "sha256:") <$>) <$> getParam "digest"
  reg2 <- gets _registry2
  msize <- v2GetImageLayerInfo reg2 (T.decodeUtf8 namespace) (T.decodeUtf8 digest)
  case msize of
    Just size -> do
      modifyResponse (setResponseStatus 200 "OK")
      modifyResponse (setContentType "application/octet-stream")
      modifyResponse setApiVersion
      modifyResponse (setHeader "Docker-Content-Digest" digest)
      modifyResponse (setContentLength size)
      -- The official registry adds Accept-Ranges, Cache-Control, Content-Length
      -- and Etag headers
    Nothing -> do
      modifyResponse (setResponseStatus 404 "Not Found")
      modifyResponse (setContentType "application/json")
      modifyResponse setApiVersion
      -- TODO The official registry sets a Content-Length (e.g. 157).

setApiVersion = setHeader "Docker-Distribution-Api-Version" "registry/2.0"

-- Used by the official registry.
setNoSniff = setHeader "X-Content-Type-Options" "nosniff"

setRange a b = setHeader "Range" (B.pack (concat [show a, "-", show b]))
