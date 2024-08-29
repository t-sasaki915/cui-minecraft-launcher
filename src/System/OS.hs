{-# LANGUAGE CPP #-}

module System.OS (OSType (..), currentOSType) where

import           Data.Aeson  (FromJSON (parseJSON), Value (String))
import           Text.Printf (printf)

data OSType = Windows | OSX | Linux deriving (Show, Eq)

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

instance FromJSON OSType where
    parseJSON (String "windows") = pure Windows
    parseJSON (String "Windows") = pure Windows
    parseJSON (String "osx")     = pure OSX
    parseJSON (String "OSX")     = pure OSX
    parseJSON (String "linux")   = pure Linux
    parseJSON (String "Linux")   = pure Linux
    parseJSON x = fail (printf "Invalid OSType structure: %s" (show x))
