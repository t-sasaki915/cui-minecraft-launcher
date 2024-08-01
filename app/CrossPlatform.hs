{-# LANGUAGE CPP #-}

module CrossPlatform
    ( OSType (..)
    , currentOSType
    , getDefaultMinecraftGameDir
    ) where

import           Imports

import           System.Directory (getHomeDirectory)

data OSType = Windows | Linux | OSX deriving (Show, Eq)

currentOSType :: OSType
#ifdef mingw32_HOST_OS
currentOSType = Windows
#endif
#ifdef linux_HOST_OS
currentOSType = Linux
#endif
#ifdef darwin_HOST_OS
currentOSType = OSX
#endif

getDefaultMinecraftGameDir :: IO FilePath
getDefaultMinecraftGameDir = do
    homeDir <- getHomeDirectory
    return $ case currentOSType of
        Windows -> homeDir </> "AppData" </> "Roaming" </> ".minecraft"
        Linux   -> homeDir </> ".minecraft"
        OSX     -> homeDir </> "Library" </> "Application Support" </> "minecraft"
