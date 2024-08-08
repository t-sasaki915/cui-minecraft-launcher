module Game.Minecraft.MinecraftLaunch (prepareMinecraftLaunch) where

import           Imports

import           AppState

import           Control.Either.Extra           (throwEither)
import           Data.ByteString                (ByteString)
import qualified Data.ByteString                as BS
import           Data.Minecraft.ClientJson
import           Data.Minecraft.VersionManifest
import           Game.Minecraft.MinecraftFiles
import           Network.Curl                   (downloadFileFromUrl,
                                                 readBSContentFromUrl)
import           System.Directory               (createDirectoryIfMissing)

createVersionDirectoryIfMissing :: MCVersionID -> AppStateT IO ()
createVersionDirectoryIfMissing versionID = do
    minecraftDir <- getMinecraftGameDir
    lift (createDirectoryIfMissing True (getMinecraftVersionDir versionID minecraftDir))

downloadAndReadClientJson :: HasCallStack => MCVersionID -> AppStateT IO ByteString
downloadAndReadClientJson versionID = do
    availableVersions <- getVersionManifest <&> versions

    case lookup versionID (map (\(MCVersion vID _ vUrl _ _) -> (vID, vUrl)) availableVersions) of
        Just clientJsonUrl -> do
            localClientJsonPath <- getMinecraftGameDir <&> getClientJsonPath versionID

            lift (doesFileExist localClientJsonPath) >>= \case
                True ->
                    lift (BS.readFile localClientJsonPath)

                False -> do
                    putStr' (printf "Downloading client.json for Minecraft %s ..." versionID)

                    lift (readBSContentFromUrl clientJsonUrl) >>= \case
                        Right clientJsonRaw -> do
                            lift (BS.writeFile localClientJsonPath clientJsonRaw)
                            putStrLn' "OK"

                            return clientJsonRaw

                        Left errMsg -> do
                            putStrLn' "ERROR"
                            error (printf "Failed to download client.json: %s" errMsg)


        Nothing ->
            error (printf "'%s' is not an available Minecraft version." versionID)

downloadAssetIndex :: HasCallStack => ClientJson -> AppStateT IO ()
downloadAssetIndex clientJson = do
    let assetVer = clientAssets clientJson
    localAssetIndexPath <- getMinecraftGameDir <&> getMinecraftAssetIndexPath assetVer

    unlessM (lift (doesFileExist localAssetIndexPath)) $ do
        let assetIndexUrl = assetUrl (clientAssetIndex clientJson)

        putStr' (printf "Downloading an asset index version %s ..." assetVer)

        lift (readBSContentFromUrl assetIndexUrl) >>= \case
            Right assetIndexRaw -> do
                lift (BS.writeFile localAssetIndexPath assetIndexRaw)
                putStrLn' "OK"

            Left errMsg -> do
                putStrLn' "ERROR"
                error (printf "Failed to download an asset index: %s" errMsg)

downloadClientJar :: HasCallStack => ClientJson -> AppStateT IO ()
downloadClientJar clientJson = do
    let clientVer = clientVersionId clientJson
    localClientJarPath <- getMinecraftGameDir <&> getClientJarPath clientVer

    unlessM (lift (doesFileExist localClientJarPath)) $ do
        let clientJarUrl = clientDownloadUrl (clientDownload (clientDownloads clientJson))

        putStr' (printf "Downloading client.jar for Minecraft %s ..." clientVer)

        lift (downloadFileFromUrl localClientJarPath clientJarUrl ) >>= \case
            Right () ->
                putStrLn' "OK"

            Left errMsg -> do
                putStrLn' "ERROR"
                error (printf "Failed to download client.jar: %s" errMsg)

    return ()

prepareMinecraftLaunch :: HasCallStack => MCVersionID -> AppStateT IO ()
prepareMinecraftLaunch versionID = do
    createVersionDirectoryIfMissing versionID

    rawClientJson <- downloadAndReadClientJson versionID
    let clientJson = throwEither (parseClientJson rawClientJson)

    downloadAssetIndex clientJson
    downloadClientJar clientJson
