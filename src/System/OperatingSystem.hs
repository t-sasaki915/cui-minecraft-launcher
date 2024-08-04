{-# LANGUAGE CPP #-}

module System.OperatingSystem
    ( OSType (..)
    , OSArch(..)
    , currentOSType
    , currentOSArch
    , fetchOSVersion
    ) where

import           Data.Aeson       (FromJSON (..), Value (..))
import           GHC.Stack        (HasCallStack)
import           System.Exit      (ExitCode (ExitSuccess))
import           System.Process   (proc, readCreateProcessWithExitCode)
import           Text.Printf      (printf)
import           Text.Regex.Posix ((=~))

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

fetchOSVersion :: HasCallStack => IO String
fetchOSVersion = case currentOSType of
    Windows -> do
        (exitCode, stdout, stderr) <- readCreateProcessWithExitCode
            (proc "systeminfo.exe" []) []

        case exitCode of
            ExitSuccess -> do
                let osNameLine = stdout =~ ("^OS Name: +Microsoft Windows .+ .+$" :: String)
                    elements   = words osNameLine -- ["OS", "Name:", "Microsoft", "Windows", "<Version>", "<Edition>"]

                return (elements !! 4)

            _ ->
                error (printf "Failed to fetch the version of Windows: %s" stderr)

    OSX -> do
        (exitCode, stdout, stderr) <- readCreateProcessWithExitCode
            (proc "sw_vers" ["-productVersion"]) []

        case exitCode of
            ExitSuccess -> do
                return stdout

            _ ->
                error (printf "Failed to fetch the version of OSX: %s" stderr)

    Linux -> do
        (exitCode, stdout, stderr) <- readCreateProcessWithExitCode
            (proc "uname" ["-r"]) []

        case exitCode of
            ExitSuccess -> do
                return stdout

            _ ->
                error (printf "Failed to fetch the version of Linux: %s" stderr)

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
