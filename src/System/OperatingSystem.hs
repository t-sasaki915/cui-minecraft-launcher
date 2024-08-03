{-# LANGUAGE CPP #-}

module System.OperatingSystem (OSType (..), currentOSType) where

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
