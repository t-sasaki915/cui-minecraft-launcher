{-# LANGUAGE CPP #-}

module System.OS.Arch (OSArch (..), currentOSArch) where

import           Data.Aeson  (FromJSON (parseJSON), Value (String))
import           Text.Printf (printf)

data OSArch = X86 | X86_64 | Arm64 deriving (Show, Eq)

currentOSArch :: OSArch
#ifdef x86_64_HOST_ARCH
currentOSArch = X86_64
#endif
#ifdef i386_HOST_ARCH
currentOSArch = X86
#endif
#if defined(arm64_HOST_ARCH) || defined(aarch64_HOST_ARCH)
currentOSArch = Arm64
#endif

instance FromJSON OSArch where
    parseJSON (String "x86_64")  = pure X86_64
    parseJSON (String "x86")     = pure X86
    parseJSON (String "arm64")   = pure Arm64
    parseJSON (String "aarch64") = pure Arm64
    parseJSON x = fail (printf "Invalid OSArch structure: %s" (show x))
