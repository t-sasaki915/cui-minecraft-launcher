{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module AppState
    ( AppState(..)
    , AppStateT
    , runAppStateT
    , getAppState
    , putAppState
    , putStrLn'
    , initialAppState
    , initialiseVersionManifestWith
    , getMinecraftGameDir
    , getVersionManifest
    ) where

import           Imports

import           AppOption                        (AppOption (..))

import           Control.Lens                     (makeLenses, set)
import           Control.Monad.Trans.State.Strict (StateT, get, put, runStateT)
import           Data.Minecraft.VersionManifest   (VersionManifest)

data AppState = AppState
    { _appOption       :: AppOption
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
putStrLn' = lift . putStrLn

initialAppState :: AppOption -> AppState
initialAppState appOpt =
    AppState
        { _appOption       = appOpt
        , _versionManifest = Nothing
        }

initialiseVersionManifestWith :: Monad m => VersionManifest -> AppStateT m ()
initialiseVersionManifestWith manifest = do
    appState <- getAppState
    putAppState (set versionManifest (Just manifest) appState)

getMinecraftGameDir :: Monad m => AppStateT m FilePath
getMinecraftGameDir = getAppState <&> (_minecraftGameDir . _appOption)

getVersionManifest :: (HasCallStack, Monad m) => AppStateT m VersionManifest
getVersionManifest = (getAppState <&> _versionManifest) >>= \case
    Just manifest -> return manifest
    Nothing       -> error "The VersionManifest has not been initialised yet."
