module System.OS.Version (OSVersion, fetchOSVersion) where

import           GHC.Stack            (HasCallStack)
import           System.OS            (OSType (..), currentOSType)
import           System.Process.Extra (readProcessEither)
import           Text.Printf          (printf)
import           Text.Regex.Posix     ((=~))

type OSVersion = String

fetchOSVersion :: HasCallStack => IO OSVersion
fetchOSVersion = case currentOSType of
    Windows ->
        readProcessEither "systeminfo.exe" [] >>= \case
            Right output ->
                let osNameLine = output =~ ("^OS Name: +Microsoft Windows .+ .+$" :: String)
                    elements = words osNameLine in -- ["OS", "Name:", "Microsoft", "Windows", "<Version>", "<Edition>"]
                        return (elements !! 4)

            Left errMsg ->
                error (printf "Failed to fetch the version of Windows: %s" errMsg)

    OSX ->
        readProcessEither "sw_vers" ["-productVersion"] >>= \case
            Right output ->
                return output

            Left errMsg ->
                error (printf "Failed to fetch the version of OSX: %s" errMsg)

    Linux ->
        readProcessEither "uname" ["-r"] >>= \case
            Right output ->
                return output

            Left errMsg ->
                error (printf "Failed to fetch the version of Linux: %s" errMsg)

