{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import Control.Exception (SomeException)
import Control.Monad (when)
import Control.Monad.CatchIO (try)
import Control.Monad.Trans (liftIO)
import qualified Data.ByteString.Char8 as BC
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Version (showVersion)
import Paths_rescoyl (version)
import Snap.Http.Server
import Snap.Snaplet
import Snap.Snaplet.Session.Backends.CookieSession
import System.Console.CmdArgs.Implicit
import System.Directory (canonicalizePath, createDirectoryIfMissing)
import System.IO (hPutStrLn, stderr)

import Rescoyl.Handlers
import Rescoyl.Simple
import Rescoyl.Types

main :: IO ()
main = (runCmd =<<) $ cmdArgs $
  modes
    [ cmdServe
    , cmdReadImageIndex
    , cmdAddUser
    ]
  &= summary versionString
  &= program "rescoyl"

-- | String with the program name, version and copyright.
versionString :: String
versionString = "rescoyl " ++ showVersion version ++
  " - Copyright (c) 2013-2105 Vo Minh Thu."

-- | Data type representing the different command-line subcommands.
data Cmd =
    CmdServe
  { cmdServeEndpoint :: String
  , cmdStore :: String
  }
  | CmdReadImageIndex
  { cmdStore :: String
  , cmdNamespace :: String
  , cmdRepository :: String
  }
  | CmdAddUser
  { cmdStore :: String
  }
  deriving (Data, Typeable)

-- | Create a 'Serve' command.
cmdServe :: Cmd
cmdServe = CmdServe
  { cmdServeEndpoint = "registry.local"
    &= explicit
    &= name "endpoint"
  , cmdStore = "store"
    &= explicit
    &= name "store"
    &= help "Path to the directory where to save files."
  } &= help "Start the HTTP server."
    &= explicit
    &= name "serve"

-- | Create a 'ReadImageIndex' command.
cmdReadImageIndex :: Cmd
cmdReadImageIndex = CmdReadImageIndex
  { cmdStore = "store"
    &= explicit
    &= name "store"
    &= help "Path to the directory where to save files."
  , cmdNamespace = def
    &= explicit
    &= name "namespace"
    &= help "Namespace for which the index must be read."
  , cmdRepository = def
    &= explicit
    &= name "repository"
    &= help "Repository for which the index must be read."
  } &= help "Read the image index (for debugging)."
    &= explicit
    &= name "read-image-index"

-- | Create a 'AddUser' command.
cmdAddUser :: Cmd
cmdAddUser = CmdAddUser
  { cmdStore = "store"
    &= explicit
    &= name "store"
    &= help "Path to the directory where to save files."
  } &= help "Add a user (prompt interactively for a password)."
    &= explicit
    &= name "add-user"

-- serveSnaplet' :: Config Snap AppConfig -> SnapletInit b b -> IO ()
-- | Similar to the standard serveSnaplet but does not parse the command-line.
serveSnaplet' config initializer = do
    (msgs, handler, doCleanup) <- runSnaplet (Just "devel") initializer
    (conf, site) <- combineConfig config handler
    createDirectoryIfMissing False "log"
    let serve = simpleHttpServe conf
    when (loggingEnabled conf) $ liftIO $ hPutStrLn stderr $ T.unpack msgs
    _ <- try $ serve $ site
         :: IO (Either SomeException ())
    doCleanup
  where
    loggingEnabled = not . (== Just False) . getVerbose

runCmd :: Cmd -> IO ()
runCmd CmdServe{..} = do
  static <- canonicalizePath cmdStore
  let config =
        setBind "0.0.0.0" $
        setPort 80 $
        defaultConfig
  serveSnaplet' config $ appInit static [cmdServeEndpoint]

runCmd CmdReadImageIndex{..} = do
  static <- canonicalizePath cmdStore
  minfo <- readImageIndex' static (BC.pack cmdNamespace)
    (BC.pack cmdRepository)
  print minfo

runCmd CmdAddUser{..} = do
  static <- canonicalizePath cmdStore
  us <- readUsers static
  (login, hashedPassword) <- makeUser
  let us' = (M.insert login hashedPassword (fst us), snd us)
  writeUsers static us'

appInit :: FilePath -> [String] -> SnapletInit App App
appInit static endpoints = makeSnaplet "rescoyl" description Nothing $ do
  s <- nestSnaplet "session" sess $
    initCookieSessionManager "session.key" "session" Nothing
  us <- liftIO $ initUserBackend static
  r <- liftIO $ initRegistryBackend static
  r2 <- liftIO $ initRegistry2Backend static
  addRoutes $ routes endpoints
  wrapSite catch500
  return $ App s us r r2

  where description = "Rescoyl, a private Docker registry."
