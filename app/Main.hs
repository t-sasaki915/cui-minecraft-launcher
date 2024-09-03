module Main (main) where

import           Interface.CommandPrompt              (startCommandPrompt)
import           Internal.AppState
import           Internal.CommandLineOption           (CommandLineOption,
                                                       getMinecraftDir_,
                                                       parseCommandLineOption)

import           Control.Monad.Trans.Class            (MonadTrans (lift))
import qualified Data.ByteString                      as BS
import           Data.JavaRuntime.JavaRuntimeManifest
import           Data.Minecraft.VersionManifestV2
import           GHC.Stack                            (HasCallStack)
import           Network.Curl                         (downloadFile)
import           System.Directory                     (createDirectoryIfMissing,
                                                       doesFileExist)
import           System.FilePath                      (takeDirectory)
import           System.IO                            (hFlush, stdout)
import           System.OS                            (currentOSType)
import           System.OS.Version                    (OSVersion,
                                                       fetchOSVersion)
import           Text.Printf                          (printf)

checkOSVersion :: IO OSVersion
checkOSVersion = do
    putStr "Checking OS version ..."
    hFlush stdout

    osVersion <- fetchOSVersion

    putStrLn (printf "%s %s" (show currentOSType) osVersion)

    return osVersion

fetchVersionManifest :: HasCallStack => CommandLineOption -> IO VersionManifestV2
fetchVersionManifest appOption = do
    putStr "Updating VersionManifest ..."
    hFlush stdout

    let localVersionManifestPath = getLocalVersionManifestV2Path (getMinecraftDir_ appOption)
    createDirectoryIfMissing True (takeDirectory localVersionManifestPath)

    downloadFile localVersionManifestPath getVersionManifestV2Url >>= \case
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
    return (either error id (parseVersionManifestV2 rawVersionManifest))

fetchJavaRuntimeManifestAll :: HasCallStack => CommandLineOption -> IO JavaRuntimeManifestAll
fetchJavaRuntimeManifestAll appOption = do
    putStr "Updating JavaRuntimeManifest ..."
    hFlush stdout

    let localJavaRuntimeManifestAllPath = getLocalJavaRuntimeManifestAllPath (getMinecraftDir_ appOption)
    createDirectoryIfMissing True (takeDirectory localJavaRuntimeManifestAllPath)

    downloadFile localJavaRuntimeManifestAllPath getJavaRuntimeManifestAllUrl >>= \case
        Right () ->
            putStrLn "OK"

        Left errMsg -> do
            putStrLn "ERROR"
            putStrLn (printf "Failed to download JavaRuntimeManifest: %s" errMsg)

            doesFileExist localJavaRuntimeManifestAllPath >>= \case
                True ->
                    putStrLn "Using old JavaRuntimeManifest instead."

                False ->
                    error "Could not find old JavaRuntimeManifest."

    rawJavaRuntimeManifestAll <- BS.readFile localJavaRuntimeManifestAllPath
    return (either error id (parseJavaRuntimeManifestAll rawJavaRuntimeManifestAll))

main :: IO ()
main = do
    appOption              <- parseCommandLineOption
    osVersion              <- checkOSVersion
    versionManifest        <- fetchVersionManifest appOption
    javaRuntimeManifestAll <- fetchJavaRuntimeManifestAll appOption

    let appState = initialiseAppState osVersion versionManifest javaRuntimeManifestAll appOption

    flip evalAppStateT appState $ do
        minecraftDir <- getMinecraftDir

        lift (putStrLn (printf "Using a directory '%s'." minecraftDir))

        startCommandPrompt
