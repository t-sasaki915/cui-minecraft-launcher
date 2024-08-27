module Main (main) where

import           Internal.AppState
import           Internal.CommandLineOption     (CommandLineOption,
                                                 getMinecraftDir_,
                                                 parseCommandLineOption)

import           Control.Monad.Trans.Class      (MonadTrans (lift))
import qualified Data.ByteString                as BS
import           Data.Minecraft.VersionManifest
import           GHC.Stack                      (HasCallStack)
import           Network.Curl                   (downloadFile)
import           System.Directory               (createDirectoryIfMissing,
                                                 doesFileExist)
import           System.FilePath                (takeDirectory)
import           System.IO                      (hFlush, stdout)
import           System.OS                      (currentOSType)
import           System.OS.Version              (OSVersion, fetchOSVersion)
import           Text.Printf                    (printf)

checkOSVersion :: IO OSVersion
checkOSVersion = do
    putStr "Checking OS version ..."
    hFlush stdout

    osVersion <- fetchOSVersion

    putStrLn (printf "%s %s" (show currentOSType) osVersion)

    return osVersion

fetchVersionManifest :: HasCallStack => CommandLineOption -> IO VersionManifest
fetchVersionManifest appOption = do
    putStr "Updating VersionManifest ..."
    hFlush stdout

    let localVersionManifestPath = getLocalVersionManifestPath (getMinecraftDir_ appOption)
    createDirectoryIfMissing True (takeDirectory localVersionManifestPath)

    downloadFile localVersionManifestPath getVersionManifestUrl >>= \case
        Right () ->
            putStrLn "OK"

        Left errMsg -> do
            putStrLn "ERROR"
            putStrLn (printf "Failed to download VersionManifest: %s" errMsg)

            doesFileExist localVersionManifestPath >>= \case
                True ->
                    putStrLn "Using old VersionManifest instead."

                False ->
                    error "Could not find old VersionManifest."

    rawVersionManifest <- BS.readFile localVersionManifestPath
    return (either error id (parseVersionManifest rawVersionManifest))

main :: IO ()
main = do
    appOption       <- parseCommandLineOption
    osVersion       <- checkOSVersion
    versionManifest <- fetchVersionManifest appOption

    let appState = initialiseAppState osVersion versionManifest appOption

    flip evalAppStateT appState $ do
        minecraftDir <- getMinecraftDir

        lift (putStrLn (printf "Using a directory '%s'." minecraftDir))
