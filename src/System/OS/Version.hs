module System.OS.Version (OSVersion, fetchOSVersion) where

import           Data.Functor         ((<&>))
import           GHC.Stack            (HasCallStack)
import           System.OS            (OSType (..), currentOSType)
import           System.Process.Extra (readProcessEither)
import           Text.Printf          (printf)
import           Text.Regex.Posix     ((=~))

type OSVersion = String

fetchOSVersion :: HasCallStack => IO OSVersion
fetchOSVersion = case currentOSType of
    Windows -> do
        readProcessEither "systeminfo.exe" [] >>= \case
            Right output ->
                let osNameLine = output =~ ("^OS Name: +Microsoft Windows .+ .+$" :: String)
                    elements = words osNameLine in -- ["OS", "Name:", "Microsoft", "Windows", "<Version>", "<Edition>"]
                        return (elements !! 4)

            Left errMsg ->
                error (printf "Failed to fetch the version of Windows: %s" errMsg)

    OSX ->
        readProcessEither "sw_vers" ["-productVersion"] <&>
            either (error . printf "Failed to fetch the version of OSX: %s") id

    Linux ->
        readProcessEither "uname" ["-r"] <&>
            either (error . printf "Failed to fetch the version of Linux: %s") id

