{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
-- | This module define simple backends using the file-system. The interfaces
-- for those backends are defined in `Rescoyl.Types`.
module Rescoyl.Simple where

import Control.Applicative ((<$>))
import Control.Exception (bracket_)
import Control.Monad.Trans (liftIO)
import Crypto.PasswordStore (makePassword, verifyPassword)
import Data.Aeson (decode, encode)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as L
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import Snap.Core
  ( finishWith, getResponse , modifyResponse
  , setResponseStatus, writeText)
import Snap.Snaplet
import System.Directory
  ( createDirectoryIfMissing, doesDirectoryExist, getDirectoryContents
  , doesFileExist
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

imagePath :: FilePath -> Text -> Text -> FilePath
imagePath static namespace image =
  static </> "v1" </> T.unpack namespace </> "images" </> T.unpack image

-- | Check if at least one repository contains a given image.
-- Return the first namespace from which the image can be obtained.
isImageInRepositories :: FilePath -> Text -> [(Text, Text)] -> IO (Maybe Text)
isImageInRepositories _ _ [] = return Nothing
isImageInRepositories static image ((namespace, repo):rest) = do
  info <- readImageIndex' static (T.encodeUtf8 namespace) (T.encodeUtf8 repo)
  case info of
    Nothing -> return Nothing
    Just images ->
      if T.unpack image `elem` map imageInfoId images
      then return (Just namespace)
      else isImageInRepositories static image rest

notFound :: Handler App App ()
notFound = do
  modifyResponse $ setResponseStatus 404 "Not Found"
  writeText "404 Not Found"
  r <- getResponse
  finishWith r

-- | Mapping login / hashed password, and list of public repositories.
type Users = (Map Text Text, [(Text, Text)])

initUserBackend :: FilePath -> IO UserBackend
initUserBackend static = do
  us <- readUsers static
  return UserBackend
    { isAuthorized = isAuthorized' us
    , isAllowedToReadImage = isAllowedToReadImage' us static
    }

readUsers :: FilePath -> IO Users
readUsers static = do
  let path = static </> "users"
  e <- liftIO $ doesFileExist path
  let path' = static </> "public-images"
  e' <- liftIO $ doesFileExist path'
  if not e || not e'
    then return (M.empty, [])
    else do
      musers <- decode <$> L.readFile path
      mimages <- decode <$> L.readFile path'
      case (musers, mimages) of
        (Nothing, _) -> error "Can't read users file."
        (_, Nothing) -> error "Can't read public-images file."
        (Just users, Just images) -> return (users, images)

writeUsers :: FilePath -> Users -> IO ()
writeUsers static us = do
  let path = static </> "users"
  L.writeFile path $ encode us

makeUser :: IO (Text, Text)
makeUser = do
  putStr "Login: " >> hFlush stdout
  login <- getLine
  putStr "Password: " >> hFlush stdout
  pw <- withEcho False getLine
  putChar '\n'
  pw' <- makePassword (B.pack pw) 16
  return (T.pack login, T.decodeUtf8 pw')

withEcho :: Bool -> IO a -> IO a
withEcho echo action = do
  old <- hGetEcho stdin
  bracket_ (hSetEcho stdin echo) (hSetEcho stdin old) action

-- | Check a login and password for authorization.
isAuthorized' :: Users -> Maybe (Text, Text) -> IO (Maybe Text)
isAuthorized' _ Nothing = return Nothing
isAuthorized' us (Just (login, password)) =
  case M.lookup login (fst us) of
    Just hashedPassword | verifyPassword (T.encodeUtf8 password) (T.encodeUtf8 hashedPassword) ->
      return $ Just login
    _ -> return Nothing

-- | Check a login and image for access rights.
-- Return a namespace from which the image can be obtained.
isAllowedToReadImage' :: Users -> FilePath -> Maybe Text -> Text
  -> IO (Maybe (Text, Authorization))
isAllowedToReadImage' us static mlogin image = do
  -- Login provided; check if image is in the login's repos or in a read-only
  -- repo.
  -- No login; check if image is in a read-only repo.
  repos <- case mlogin of
    Just login -> map (login,) <$>
      listRepositories static (T.encodeUtf8 login)
    Nothing -> return []
  mnamespace <- isImageInRepositories static image (repos ++ snd us)
  case mnamespace of
    Just namespace | mnamespace == mlogin ->
      return (Just (namespace, ReadWrite))
    Just namespace -> return (Just (namespace, ReadOnly))
    Nothing -> return Nothing

initRegistryBackend :: FilePath -> IO RegistryBackend
initRegistryBackend static = do
  return RegistryBackend
    { loadImage = loadImage' static
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

loadImage' :: FilePath -> Text -> Text -> IO GetImage
loadImage' static namespace image = do
  let dir = imagePath static namespace image
      path = dir </> "json"
      path' = dir </> "layer"
      path'' = dir </> "checksum"
      path''' = dir </> "client_checksum"

  e <- doesFileExist path
  a <- doesFileExist $ dir </> "ancestry"
  if e && a
    then do
      mjson <- decode <$> L.readFile path
      mjson' <- decode <$> L.readFile (dir </> "ancestry")
      case (mjson, mjson') of
        (Nothing, _) -> return ImageErrorDecodingJson
        (_, Nothing) -> return ImageErrorDecodingJson
        (Just json, Just ancestry) -> do
          e' <- doesFileExist path'
          e'' <- doesFileExist path''
          if e' && e''
            then do
              size <- getFileStatus path' >>= return . fileSize
              checksum <- B.readFile path''
              e''' <- doesFileExist path'''
              if e'''
                then do
                  clientChecksum <- B.readFile path'''
                  return $ Image json ancestry
                    (Layer path' (fromIntegral size) checksum) clientChecksum
                else
                  return $ ImageLayer json ancestry
                    (Layer path' (fromIntegral size) checksum)
            else return $ ImageJson json ancestry
    else return ImageDoesntExist

-- TODO `desc` is actually `decode content`. This means that normally
-- `content` is redundant. But right now, `decode content` is lossy
-- and we really want to store the whole `content`.
saveImageJson' :: FilePath -> Text -> Text
  -> ImageDescription -> L.ByteString -> IO ()
saveImageJson' static namespace image desc content = do
  let dir = imagePath static namespace image
  createDirectoryIfMissing True dir
  L.writeFile (dir </> "json") content
  generateAncestry static namespace image $ imageDescriptionParent desc

generateAncestry :: String -> Text -> Text -> Maybe String -> IO ()
generateAncestry static namespace image mparent = do
  parents <- case mparent of
    Nothing -> return []
    Just parent -> do
      let dir' = imagePath static namespace $ T.pack parent
      mparents <- decode <$> (L.readFile $ dir' </> "ancestry")
      case mparents of
        Nothing -> error "Corrupted parent ancestry file."
        Just parents -> return parents
  let dir = imagePath static namespace image
  L.writeFile (dir </> "ancestry") $ encode $ image : parents

saveImageLayer' :: FilePath -> Text -> Text -> Handler App App ()
saveImageLayer' static namespace image = do
  let dir = imagePath static namespace image
  -- TODO how to bracket open/close with iterHandle in between ?
  -- TODO Ensure the json is already saved.
  json <- liftIO $ B.readFile (dir </> "json")
  (checksum, _) <- saveImageLayerToFile json (dir </> "layer")
  liftIO $ B.writeFile (dir </> "checksum") $ "sha256:" `B.append` checksum

saveImageChecksum' :: FilePath -> Text -> Text -> ByteString -> IO ()
saveImageChecksum' static namespace image checksum = do
  let dir = imagePath static namespace image
  B.writeFile (dir </> "client_checksum") checksum

saveImageChecksumOld' :: FilePath -> Text -> Text -> ByteString -> IO ()
saveImageChecksumOld' static namespace image checksum = do
  let dir = imagePath static namespace image
  B.writeFile (dir </> "checksum") checksum
  B.writeFile (dir </> "client_checksum") checksum

saveRepository' :: FilePath -> ByteString -> ByteString -> [ImageInfo] -> IO ()
saveRepository' static namespace repo images = do
  let dir = repositoryPath static namespace repo
  liftIO $ do
    createDirectoryIfMissing True $ dir </> "tags"
    L.writeFile (dir </> "images") $ encode images

readTags' :: FilePath -> ByteString -> ByteString -> IO [(Text, Text)]
readTags' static namespace repo = do
  let dir = repositoryPath static namespace repo
  names <- do
    e <- doesDirectoryExist dir
    if e
      then getDirectoryContents $ dir </> "tags"
      else return []
  let names' = filter (not . (`elem` [".", ".."])) names
  tags <- mapM (T.readFile . (\n -> dir </> "tags" </> n)) names'
  return $ zipWith (\a b -> (T.pack a, b)) names' tags

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

listRepositories :: FilePath -> ByteString -> IO [Text]
listRepositories static namespace = do
  let dir = static </> "v1" </> B.unpack namespace </> "repositories"
  names <- do
    e <- doesDirectoryExist dir
    if e
      then getDirectoryContents dir
      else return []
  let names' = filter (not . (`elem` [".", ".."])) names
  return (map T.pack names')
