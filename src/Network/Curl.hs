module Network.Curl
    ( Url
    , readContentFromUrl
    , readBSContentFromUrl
    , downloadFile
    ) where

import           Data.ByteString          (ByteString, pack)
import           Data.ByteString.Internal (c2w)
import           Data.Functor             ((<&>))
import           Data.List.Extra          (dropEnd)
import           System.OS                (OSType (..), currentOSType)
import           System.Process.Extra     (execProcessEither, readProcessEither)

type Url = String

curlExecName :: FilePath
curlExecName = case currentOSType of
    Windows -> "curl.exe"
    OSX     -> "curl"
    Linux   -> "curl"

readContentFromUrl :: Url -> IO (Either String String)
readContentFromUrl url =
    readProcessEither curlExecName ["--fail", "--silent", "--show-error", url] <&>
        either (Left . removeLastNewLine) Right

readBSContentFromUrl :: Url -> IO (Either String ByteString)
readBSContentFromUrl url =
    readContentFromUrl url <&> fmap (pack . map c2w)

downloadFile :: FilePath -> Url -> IO (Either String ())
downloadFile downloadPath url =
    execProcessEither curlExecName ["--fail", "--silent", "--show-error", "--output", downloadPath, url] <&>
        either (Left . removeLastNewLine) Right

removeLastNewLine :: String -> String
removeLastNewLine str = case last str of
    '\n' -> dropEnd 1 str
    _    -> str
