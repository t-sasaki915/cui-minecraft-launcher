module System.OS.Version (OSVersion, fetchOSVersion) where

import           Data.Functor         ((<&>))
import           GHC.Stack            (HasCallStack)
import           System.OS            (OSType (..), currentOSType)
import           System.Process.Extra2 (readProcessEither)
import           Text.Printf          (printf)
import           Text.Regex.Posix     ((=~))

type OSVersion = String

fetchOSVersion :: HasCallStack => IO OSVersion
fetchOSVersion = case currentOSType of
    Windows ->
        readProcessEither "wmic.exe" ["os", "get", "version"] <&>
            either (error . printf "Failed to fetch the version of Windows: %s")
                (=~ ("^[0-9]+\\.[0-9]+\\..+$" :: String))

    OSX ->
        readProcessEither "sw_vers" ["-productVersion"] <&>
            either (error . printf "Failed to fetch the version of OSX: %s") id

    Linux ->
        readProcessEither "uname" ["-r"] <&>
            either (error . printf "Failed to fetch the version of Linux: %s") id

