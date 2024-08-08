module Network.Curl
    ( curlExecutableName
    , readContentFromUrl
    , readBSContentFromUrl
    , downloadFileFromUrl
    ) where

import           Data.ByteString          (ByteString, pack)
import           Data.ByteString.Internal (c2w)
import           Data.String.Extra        (removeLastNewLine)
import           System.Exit              (ExitCode (ExitSuccess))
import           System.OperatingSystem   (OSType (..), currentOSType)
import           System.Process           (proc, readCreateProcessWithExitCode)

type Url = String

curlExecutableName :: String
curlExecutableName = case currentOSType of
    Windows -> "curl.exe"
    Linux   -> "curl"
    OSX     -> "curl"

readContentFromUrl :: Url -> IO (Either String String)
readContentFromUrl url = do
    let
        curlArgs =
            [ "--fail"
            , "--silent"
            , "--show-error"
            , url
            ]

    (exitCode, stdout, stderr) <-
        readCreateProcessWithExitCode
            (proc curlExecutableName curlArgs) []

    case exitCode of
        ExitSuccess ->
            return (Right stdout)

        _ ->
            return (Left (removeLastNewLine stderr))

readBSContentFromUrl :: Url -> IO (Either String ByteString)
readBSContentFromUrl url =
    readContentFromUrl url >>=
        either (return . Left) (return . Right . pack . map c2w)

downloadFileFromUrl :: FilePath -> Url -> IO (Either String ())
downloadFileFromUrl downloadPath url = do
    let
        curlArgs =
            [ "--fail"
            , "--silent"
            , "--show-error"
            , "--output"
            , downloadPath
            , url
            ]

    (exitCode, _, stderr) <-
        readCreateProcessWithExitCode
            (proc curlExecutableName curlArgs) []

    case exitCode of
        ExitSuccess ->
            return (Right ())

        _ ->
            return (Left (removeLastNewLine stderr))
