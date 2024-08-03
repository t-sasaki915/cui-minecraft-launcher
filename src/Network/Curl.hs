module Network.Curl (curlExecutableName) where

import           System.OperatingSystem (OSType (..), currentOSType)

curlExecutableName :: String
curlExecutableName = case currentOSType of
    Windows -> "curl.exe"
    Linux   -> "curl"
    OSX     -> "curl"
