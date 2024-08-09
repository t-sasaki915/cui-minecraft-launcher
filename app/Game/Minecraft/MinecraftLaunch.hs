module Game.Minecraft.MinecraftLaunch (prepareMinecraftLaunch) where

import           Imports

import           AppState

import           Control.Concurrent.Async       (forConcurrently_)
import           Control.Either.Extra           (throwEither)
import           Data.Aeson
import qualified Data.Aeson.KeyMap              as KeyMap
import           Data.ByteString                (ByteString)
import qualified Data.ByteString                as BS
import           Data.List.Extra                (chunksOf)
import           Data.Minecraft.ClientJson
import           Data.Minecraft.VersionManifest
import           Data.Text                      (unpack)
import           Game.Minecraft.MinecraftFiles
import           Game.Minecraft.MojangConstants (getMinecraftAssetObjectDownloadUrl)
import           Network.Curl                   (downloadFileFromUrl,
                                                 readBSContentFromUrl)
import           System.Directory               (createDirectoryIfMissing)
import           System.ProgressBar

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

analyseAssetHashes :: HasCallStack => ByteString -> [String]
analyseAssetHashes assetIndexRaw = do
    case eitherDecodeStrict' assetIndexRaw :: Either String Object of
        Right obj ->
            case KeyMap.lookup "objects" obj of
                Just (Object objs) ->
                    flip map (KeyMap.elems objs) $ \case
                        (Object obj') ->
                            case KeyMap.lookup "hash" obj' of
                                Just (String hash) ->
                                    unpack hash

                                _ ->
                                    error "Could not find a hash key in the asset index."

                        _ ->
                            error "Could not find a hash key in the asset index."

                _ ->
                    error "Could not find a objects key in the asset index."

        Left errMsg ->
            error (printf "Failed to parse the asset index: %s" errMsg)

downloadAssets :: HasCallStack => [String] -> AppStateT IO ()
downloadAssets assetHashes = do
    minecraftDir <- getMinecraftGameDir

    let
        progressBarStyle =
            Style
                { styleWidth         = TerminalWidth 100
                , styleTodo          = '.'
                , stylePrefix        = "Downloading assets"
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

    assetHashesToDownload <-
        flip filterM assetHashes $ \assetHash -> do
            let localAssetObjectPath = getMinecraftAssetObjectPath assetHash minecraftDir
            lift (doesFileExist localAssetObjectPath) <&> not

    unless (null assetHashesToDownload) $ do
        progressBar <- lift (newProgressBar progressBarStyle 10 (Progress 0 (length assetHashesToDownload) ()))

        let assetChunks = chunksOf 80 assetHashesToDownload

        lift $ forM_ assetChunks $ \assetChunk ->
            forConcurrently_ assetChunk $ \assetHash -> do
                let localAssetObjectPath = getMinecraftAssetObjectPath assetHash minecraftDir
                    assetObjectPrefixDir = getMinecraftAssetObjectPrefixDir assetHash minecraftDir
                    assetObjectUrl = getMinecraftAssetObjectDownloadUrl assetHash

                createDirectoryIfMissing True assetObjectPrefixDir

                downloadFileFromUrl localAssetObjectPath assetObjectUrl >>= \case
                    Right () ->
                        incProgress progressBar 1

                    Left errMsg ->
                        error (printf "Failed to download an asset '%s': %s." assetHash errMsg)

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

    assetIndex <- downloadAssetIndex clientJson
    let assetHashes = analyseAssetHashes assetIndex
    downloadAssets assetHashes

    downloadClientJar clientJson
