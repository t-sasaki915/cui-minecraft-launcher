{-# LANGUAGE CPP #-}

module System.OperatingSystem (OSType (..), OSArch(..), currentOSType, currentOSArch) where

import           Data.Aeson  (FromJSON (..), Value (..))
import           Text.Printf (printf)

data OSType = Windows | OSX | Linux deriving (Show, Eq)
data OSArch = X86 | X86_64 deriving (Show, Eq)

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

currentOSArch :: OSArch
#ifdef x86_64_HOST_ARCH
currentOSArch = X86_64
#endif
#ifdef i386_HOST_ARCH
currentOSArch = X86
#endif

instance FromJSON OSType where
    parseJSON (String "windows") = pure Windows
    parseJSON (String "Windows") = pure Windows
    parseJSON (String "osx")     = pure OSX
    parseJSON (String "OSX")     = pure OSX
    parseJSON (String "linux")   = pure Linux
    parseJSON (String "Linux")   = pure Linux
    parseJSON x = fail (printf "Invalid OSType structure: %s" (show x))

instance FromJSON OSArch where
    parseJSON (String "x86_64") = pure X86_64
    parseJSON (String "x86")    = pure X86
    parseJSON x = fail (printf "Invalid OSArch structure: %s" (show x))
