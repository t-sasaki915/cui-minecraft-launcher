module Game.Minecraft.MinecraftLaunch (prepareMinecraftLaunch) where

import           Imports

import           AppState

import           Control.Concurrent.Async       (forConcurrently_)
import           Control.Either.Extra           (throwEither)
import           Data.ByteString                (ByteString)
import qualified Data.ByteString                as BS
import           Data.List.Extra                (chunksOf)
import           Data.Maybe                     (fromMaybe)
import           Data.Minecraft.AssetIndex
import           Data.Minecraft.ClientJson      hiding (AssetIndex)
import           Data.Minecraft.VersionManifest
import           Game.Minecraft.MinecraftFiles
import           Game.Minecraft.MojangConstants (getMinecraftAssetObjectDownloadUrl)
import           Network.Curl                   (downloadFileFromUrl,
                                                 readBSContentFromUrl)
import           System.Directory               (createDirectoryIfMissing)
import           System.FilePath                (takeDirectory)
import           System.ProgressBar

progressBarStyle :: Label s -> Style s
progressBarStyle title =
    Style
        { styleWidth         = TerminalWidth 100
        , styleTodo          = '.'
        , stylePrefix        = title
        , stylePostfix       = percentage
        , styleOpen          = "["
        , styleOnComplete    = WriteNewline
        , styleEscapeTodo    = const ""
        , styleEscapePrefix  = const ""
        , styleEscapePostfix = const ""
        , styleEscapeOpen    = const ""
        , styleEscapeDone    = const ""
        , styleEscapeCurrent = const ""
        , styleEscapeClose   = const ""
        , styleDone          = '='
        , styleCurrent       = '>'
        , styleClose         = "]"
        }

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

downloadAssetIndex :: HasCallStack => ClientJson -> AppStateT IO ByteString
downloadAssetIndex clientJson = do
    let assetVer = clientAssets clientJson
    localAssetIndexPath <- getMinecraftGameDir <&> getMinecraftAssetIndexPath assetVer

    lift (doesFileExist localAssetIndexPath) >>= \case
        True ->
            lift (BS.readFile localAssetIndexPath)

        False -> do
            let assetIndexUrl = assetUrl (clientAssetIndex clientJson)

            putStr' (printf "Downloading an asset index version %s ..." assetVer)

            lift (readBSContentFromUrl assetIndexUrl) >>= \case
                Right assetIndexRaw -> do
                    lift (BS.writeFile localAssetIndexPath assetIndexRaw)
                    putStrLn' "OK"
                    return assetIndexRaw

                Left errMsg -> do
                    putStrLn' "ERROR"
                    error (printf "Failed to download an asset index: %s" errMsg)

downloadAssetObjects :: HasCallStack => ClientJson -> AssetIndex -> AppStateT IO ()
downloadAssetObjects clientJson assetIndex = do
    minecraftDir <- getMinecraftGameDir
    let versionID            = clientVersionId clientJson
        assetVersion         = clientAssets clientJson
        shouldMapToResources = fromMaybe False (mapToResources assetIndex)
        isVirtualAssets      = fromMaybe False (virtualAsset assetIndex)

    let progBarStyle = progressBarStyle "Downloading assets"

    assetObjectsToDownload <-
        flip filterM (getAssetObjects assetIndex) $ \(AssetObject assetFile assetHash_) ->
            let localAssetObjectPath
                    | shouldMapToResources = getMinecraftResourcePath assetFile versionID minecraftDir
                    | isVirtualAssets      = getMinecraftVirtualAssetObjectPath assetFile assetVersion minecraftDir
                    | otherwise            = getMinecraftAssetObjectPath assetHash_ minecraftDir in
                lift (doesFileExist localAssetObjectPath) <&> not

    unless (null assetObjectsToDownload) $ do
        progressBar <- lift (newProgressBar progBarStyle 10 (Progress 0 (length assetObjectsToDownload) ()))

        let assetChunks = chunksOf 80 assetObjectsToDownload

        lift $ forM_ assetChunks $ \assetChunk ->
            forConcurrently_ assetChunk $ \(AssetObject assetFile assetHash_) -> do
                let localAssetObjectPath
                        | shouldMapToResources = getMinecraftResourcePath assetFile versionID minecraftDir
                        | isVirtualAssets      = getMinecraftVirtualAssetObjectPath assetFile assetVersion minecraftDir
                        | otherwise            = getMinecraftAssetObjectPath assetHash_ minecraftDir
                    assetObjectUrl = getMinecraftAssetObjectDownloadUrl assetHash_

                createDirectoryIfMissing True (takeDirectory localAssetObjectPath)

                downloadFileFromUrl localAssetObjectPath assetObjectUrl >>= \case
                    Right () ->
                        incProgress progressBar 1

                    Left errMsg ->
                        error (printf "Failed to download an asset '%s': %s." assetHash_ errMsg)

downloadLibraries :: HasCallStack => ClientJson -> AppStateT IO ()
downloadLibraries clientJson = do
    currentOSVersion <- getOSVersion
    minecraftDir     <- getMinecraftGameDir

    let ruleContext = RuleContext
            { osVersion = currentOSVersion
            , isDemoUser = False
            , hasCustomResolution = False
            }
        adoptedLibs = processLibraries ruleContext (clientLibraries clientJson)

    librariesToDownload <-
        flip filterM adoptedLibs $ \(LibraryArtifact artPath _) ->
            let localLibraryPath = getMinecraftLibraryPath artPath minecraftDir in
                lift (doesFileExist localLibraryPath) <&> not

    let progBarStyle = progressBarStyle "Downloading libraries"

    unless (null librariesToDownload) $ do
        progressBar <- lift (newProgressBar progBarStyle 10 (Progress 0 (length librariesToDownload) ()))

        lift $ forConcurrently_ librariesToDownload $ \(LibraryArtifact artPath artUrl) -> do
            let localLibraryPath = getMinecraftLibraryPath artPath minecraftDir

            createDirectoryIfMissing True (takeDirectory localLibraryPath)

            downloadFileFromUrl localLibraryPath artUrl >>= \case
                Right () ->
                    incProgress progressBar 1

                Left errMsg ->
                    error (printf "Failed to download a library '%s': %s" artPath errMsg)

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

prepareMinecraftLaunch :: HasCallStack => MCVersionID -> AppStateT IO ()
prepareMinecraftLaunch versionID = do
    createVersionDirectoryIfMissing versionID

    rawClientJson <- downloadAndReadClientJson versionID
    let clientJson = throwEither (parseClientJson rawClientJson)

    rawAssetIndex <- downloadAssetIndex clientJson
    let assetIndex = throwEither (parseAssetIndex rawAssetIndex)
    downloadAssetObjects clientJson assetIndex

    downloadLibraries clientJson
    downloadClientJar clientJson
