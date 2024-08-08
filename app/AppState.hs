{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module AppState
    ( AppState(..)
    , AppStateT
    , runAppStateT
    , getAppState
    , putAppState
    , putStrLn'
    , putStr'
    , initialAppState
    , initialiseOSVersionWith
    , initialiseVersionManifestWith
    , getMinecraftGameDir
    , getVersionManifest
    , getOSVersion
    ) where

import           Imports

import           AppOption                        (AppOption (..))

import           Control.Lens                     (makeLenses, set)
import           Control.Monad.Trans.State.Strict (StateT, get, put, runStateT)
import           Data.Minecraft.VersionManifest   (VersionManifest)
import           System.IO                        (hFlush, stdout)

data AppState = AppState
    { _appOption       :: AppOption
    , _osVersion       :: Maybe String
    , _versionManifest :: Maybe VersionManifest
    }

makeLenses ''AppState

type AppStateT = StateT AppState

runAppStateT :: AppStateT m a -> AppState -> m (a, AppState)
runAppStateT = runStateT

getAppState :: Monad m => AppStateT m AppState
getAppState = get

putAppState :: Monad m => AppState -> AppStateT m ()
putAppState = put

putStrLn' :: String -> AppStateT IO ()
putStrLn' str = lift $ do
   putStrLn str
   hFlush stdout

putStr' :: String -> AppStateT IO ()
putStr' str = lift $ do
    putStr str
    hFlush stdout

initialAppState :: AppOption -> AppState
initialAppState appOpt =
    AppState
        { _appOption       = appOpt
        , _osVersion       = Nothing
        , _versionManifest = Nothing
        }

initialiseOSVersionWith :: Monad m => String -> AppStateT m ()
initialiseOSVersionWith osVer = do
    appState <- getAppState
    putAppState (set osVersion (Just osVer) appState)

initialiseVersionManifestWith :: Monad m => VersionManifest -> AppStateT m ()
initialiseVersionManifestWith manifest = do
    appState <- getAppState
    putAppState (set versionManifest (Just manifest) appState)

getMinecraftGameDir :: Monad m => AppStateT m FilePath
getMinecraftGameDir = getAppState <&> (_minecraftGameDir . _appOption)

getVersionManifest :: (HasCallStack, Monad m) => AppStateT m VersionManifest
getVersionManifest = (getAppState <&> _versionManifest) >>= \case
    Just manifest -> return manifest
    Nothing       -> error "_versionManifest has not been initialised yet."

getOSVersion :: (HasCallStack, Monad m) => AppStateT m String
getOSVersion = (getAppState <&> _osVersion) >>= \case
    Just osVer -> return osVer
    Nothing    -> error "_osVersion has not been initialised yet."
