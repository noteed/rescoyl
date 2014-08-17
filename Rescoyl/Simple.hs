{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
-- | This module define simple backends using the file-system. The interfaces
-- for those backends are defined in `Rescoyl.Types`.
module Rescoyl.Simple where

import Control.Applicative ((<$>))
import Control.Exception (bracket_)
import Crypto.PasswordStore (makePassword, verifyPassword)
import Data.Aeson (decode, encode, object, (.=), Value)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as L
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Text as T
import Snap (liftIO)
import Snap.Core
  ( finishWith, getResponse , modifyResponse
  , setResponseStatus, writeText , MonadSnap(..))
import Snap.Snaplet
import Snap.Util.FileServe (serveFile)
import System.Directory
  ( createDirectoryIfMissing, doesDirectoryExist, getDirectoryContents
  , removeFile, doesFileExist
  )
import System.FilePath ((</>))
import System.IO (hFlush, hGetEcho, hSetEcho, stdin, stdout)
import System.Posix (fileSize, getFileStatus)

import Rescoyl.Types
import Rescoyl.Utils (saveImageLayerToFile)

-- TODO Instead of generating the 404s, or 50Xs here, return a data type
-- representing the failure.

repositoryPath :: FilePath -> B.ByteString -> B.ByteString -> FilePath
repositoryPath static namespace repo =
  static </> "v1" </> B.unpack namespace </> "repositories" </> B.unpack repo

imagePath :: FilePath -> B.ByteString -> B.ByteString -> FilePath
imagePath static namespace image =
  static </> "v1" </> B.unpack namespace </>"images" </> B.unpack image

notFound :: Handler App App ()
notFound = do
  modifyResponse $ setResponseStatus 404 "Not Found"
  writeText "404 Not Found"
  r <- getResponse
  finishWith r

-- | Mapping login / hashed password.
type Users = Map ByteString ByteString

initUserBackend :: FilePath -> IO UserBackend
initUserBackend static = do
  us <- readUsers static
  return UserBackend
    { isAuthorized = isAuthorized' us
    }

readUsers :: FilePath -> IO Users
readUsers static = do
  let path = static </> "users"
  e <- liftIO $ doesFileExist path
  if not e
    then return M.empty
    else do
      musers <- decode <$> L.readFile path
      maybe (error "Can't read users file.") return musers

writeUsers :: FilePath -> Users -> IO ()
writeUsers static us = do
  let path = static </> "users"
  L.writeFile path $ encode us

makeUser :: IO (ByteString, ByteString)
makeUser = do
  putStr "Login: " >> hFlush stdout
  login <- getLine
  putStr "Password: " >> hFlush stdout
  pw <- withEcho False getLine
  putChar '\n'
  pw' <- makePassword (B.pack pw) 16
  return (B.pack login, pw')

withEcho :: Bool -> IO a -> IO a
withEcho echo action = do
  old <- hGetEcho stdin
  bracket_ (hSetEcho stdin echo) (hSetEcho stdin old) action

-- | Check a login and password for authorization.
isAuthorized' :: Users -> Maybe (ByteString, ByteString) -> IO (Maybe ByteString)
isAuthorized' _ Nothing = return Nothing
isAuthorized' us (Just (login, password)) =
  case M.lookup login us of
    Just hashedPassword | verifyPassword password hashedPassword ->
      return $ Just login
    _ -> return Nothing

initRegistryBackend :: FilePath -> IO RegistryBackend
initRegistryBackend static = do
  return RegistryBackend
    { imageAncestry = imageAncestry' static
    , imageJson = imageJson' static
    , imageLayer = imageLayer' static
    , saveImageJson = saveImageJson' static
    , saveImageLayer = saveImageLayer' static
    , saveImageChecksum = saveImageChecksum' static
    , saveImageChecksumOld = saveImageChecksumOld' static
    , saveRepository = saveRepository' static
    , readTags = readTags' static
    , saveTag = saveTag' static
    , readImageIndex = readImageIndex' static
    , saveImageIndex = saveImageIndex' static
    }

imageAncestry' :: FilePath -> ByteString -> ByteString -> IO (Maybe Value)
imageAncestry' static namespace image = do
  let dir = imagePath static namespace image
      path = dir </> "ancestry"

  e <- liftIO $ doesFileExist path
  if e
    then decode <$> L.readFile path
    else return Nothing

imageJson' :: FilePath -> ByteString -> ByteString -> IO GetJson
imageJson' static namespace image = do
  let dir = imagePath static namespace image
      path = dir </> "json"
      path' = dir </> "layer"

  e <- liftIO $ doesFileExist $ dir </> "_inprogress"
  if e
    then return UploadInProgress
    else do
      e' <- doesFileExist path
      e'' <- doesFileExist path'
      if e' && e''
        then do
          size <- getFileStatus path' >>= return . fileSize
          mjson <- decode <$> L.readFile path
          case mjson of
            Nothing -> return ErrorDecodingJson
            Just json -> return $ SizeAndJson (fromIntegral size) json
        else return NoSuchImage

imageLayer' :: FilePath -> ByteString -> ByteString -> Handler App App ()
imageLayer' static namespace image = do
  let dir = imagePath static namespace image
  e <- liftIO $ doesFileExist $ dir </> "_inprogress"
  if e
    then modifyResponse $
      setResponseStatus 400 "Image upload is in progress."
    else serveImageFile static namespace image "layer"

-- TODO `desc` is actually `decode content`. This means that normally
-- `content` is redundant. But right now, `decode content` is lossy
-- and we really want to store the whole `content`.
saveImageJson' :: FilePath -> ByteString -> ByteString
  -> ImageDescription -> L.ByteString -> Handler App App ()
saveImageJson' static namespace image desc content = do
  let dir = imagePath static namespace image
  liftIO $ do
    createDirectoryIfMissing True dir
    L.writeFile (dir </> "json") content
    B.writeFile (dir </> "_inprogress") "true"
  generateAncestry static namespace image $ imageDescriptionParent desc

-- | This can fail with a HTTP 500.
generateAncestry :: MonadSnap m => String -> ByteString -> ByteString -> Maybe String -> m ()
generateAncestry static namespace image mparent = do
  parents <- case mparent of
    Nothing -> return []
    Just parent -> do
      let dir' = imagePath static namespace $ B.pack parent
      mparents <- decode <$> liftIO (L.readFile $ dir' </> "ancestry")
      case mparents of
        Nothing -> do
          modifyResponse $ setResponseStatus 500 "Error decoding parent ancestry."
          r <- getResponse
          finishWith r
        Just parents -> return parents
  let dir = imagePath static namespace image
  liftIO $ L.writeFile (dir </> "ancestry") $ encode $ image : parents

saveImageLayer' :: FilePath -> ByteString -> ByteString -> Handler App App ()
saveImageLayer' static namespace image = do
  let dir = imagePath static namespace image
      path = dir </> "layer"
  -- TODO how to bracket open/close with iterHandle in between ?
  liftIO $ createDirectoryIfMissing True dir
  -- TODO Ensure the json is already saved.
  json <- liftIO $ B.readFile (dir </> "json")

  (checksum, _) <- saveImageLayerToFile json path
  liftIO $ B.writeFile (dir </> "checksum") $ "sha256:" `B.append` checksum

saveImageChecksum' :: FilePath -> ByteString -> ByteString -> ByteString -> IO PutChecksum
saveImageChecksum' static namespace image checksum = do
  let dir = imagePath static namespace image
      path = dir </> "checksum"
  e <- doesDirectoryExist dir
  if e
    then do
      e' <- liftIO $ doesFileExist $ dir </> "_inprogress"
      if e'
        then do
          -- TODO Could it be that the file doesn't exist, e.g. if the layer wasn't already pushed ?
          -- Testing that the directory exists is not enough; we should check the layer exists too.
          computed <- B.readFile path
          if computed == checksum
            then do
              removeFile $ dir </> "_inprogress"
              return ChecksumSaved
            else return ChecksumMismatch
        else return ChecksumAlreadySaved
    else return ChecksumNoSuchImage

-- | We didn't compute the checksum when the layer was uploaded so we never
-- return `ChecksumMismatch`.
saveImageChecksumOld' :: FilePath -> ByteString -> ByteString -> ByteString -> IO PutChecksum
saveImageChecksumOld' static namespace image checksum = do
  let dir = imagePath static namespace image
      path = dir </> "checksum"
  e <- doesDirectoryExist dir
  if e
    then do
      e' <- liftIO $ doesFileExist $ dir </> "_inprogress"
      if e'
        then do
          B.writeFile path checksum
          removeFile $ dir </> "_inprogress"
          return ChecksumSaved
        else return ChecksumAlreadySaved
    else return ChecksumNoSuchImage

saveRepository' :: FilePath -> ByteString -> ByteString -> [ImageInfo] -> IO ()
saveRepository' static namespace repo images = do
  let dir = repositoryPath static namespace repo
  liftIO $ do
    createDirectoryIfMissing True $ dir </> "tags"
    L.writeFile (dir </> "images") $ encode images

readTags' :: FilePath -> ByteString -> ByteString -> IO Value
readTags' static namespace repo = do
  let dir = repositoryPath static namespace repo
  names <- do
    e <- doesDirectoryExist dir
    if e
      then getDirectoryContents $ dir </> "tags"
      else return []
  let names' = filter (not . (`elem` [".", ".."])) names
  tags <- mapM (L.readFile . (\n -> dir </> "tags" </> n)) names'
  return $ object $ zipWith (\a b -> T.pack a .= b) names' tags

saveTag' :: FilePath -> ByteString -> ByteString
  -> ByteString -> L.ByteString -> IO ()
saveTag' static namespace repo tag value = do
  let dir = repositoryPath static namespace repo
  createDirectoryIfMissing True $ dir </> "tags"
  -- The body is a JSON string, which is an invalid JSON
  -- payload as per the JSON standard (which only allows
  -- list and dictionaries at the top level).
  -- We "decode" it by hand.
  L.writeFile (dir </> "tags" </> B.unpack tag) value

readImageIndex' :: FilePath -> ByteString -> ByteString -> IO (Maybe [ImageInfo])
readImageIndex' static namespace repo = do
  let dir = repositoryPath static namespace repo
      path = dir </> "images"
  e <- doesFileExist path
  if e
    then decode <$> L.readFile path
    else return $ Just []

saveImageIndex' :: FilePath -> ByteString -> ByteString -> [ImageInfo] -> Handler App App ()
saveImageIndex' static namespace repo new = do
  let dir = repositoryPath static namespace repo
      path = dir </> "images"
  moriginal <- liftIO $ readImageIndex' static namespace repo
  case moriginal of
    Nothing -> modifyResponse $
      setResponseStatus 500 "Error reading images index."
    Just original -> do
      liftIO $ L.writeFile path $ encode $ combineImageInfo original new
      modifyResponse $ setResponseStatus 204 "No Content"

serveImageFile :: FilePath -> ByteString -> ByteString -> FilePath -> Handler App App ()
serveImageFile static namespace image file = do
  let dir = imagePath static namespace image
  serveFile' $ dir </> file

serveFile' :: FilePath -> Handler App App ()
serveFile' path = do
  e <- liftIO $ doesFileExist path
  if e
    then serveFile path
    else modifyResponse $ setResponseStatus 404 "No Found"
