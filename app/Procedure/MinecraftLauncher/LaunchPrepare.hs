module Procedure.MinecraftLauncher.LaunchPrepare (prepareMinecraftLaunch) where

import           Internal.AppState                (AppStateT, getMinecraftDir,
                                                   getOSVersion)

import           Control.Concurrent.Async         (forConcurrently_)
import           Control.Monad.Extra              (filterM, forM_, unless,
                                                   unlessM)
import           Control.Monad.Trans.Class        (lift)
import qualified Data.ByteString                  as BS
import           Data.Functor                     ((<&>))
import           Data.List.Extra                  (chunksOf)
import           Data.Minecraft.AssetIndex
import           Data.Minecraft.ClientJson
import           Data.Minecraft.VersionManifestV2
import           GHC.Stack                        (HasCallStack)
import           Network.Curl                     (downloadFile)
import           System.Directory                 (createDirectoryIfMissing,
                                                   doesFileExist)
import           System.FilePath                  (takeDirectory)
import           System.IO                        (hFlush, stdout)
import           System.ProgressBar               (incProgress)
import           System.ProgressBar.Extra         (newSimpleProgressBar)
import           Text.Printf                      (printf)

downloadClientJson :: HasCallStack => MCVersion -> AppStateT IO ()
downloadClientJson mcVersion = do
    minecraftDir <- getMinecraftDir

    let clientJsonUrl = getClientJsonUrl mcVersion
        localClientJsonPath = getLocalClientJsonPath minecraftDir mcVersion

    unlessM (lift (doesFileExist localClientJsonPath)) $ do
        lift (putStr "Downloading ClientJson ...")
        lift (hFlush stdout)

        lift (createDirectoryIfMissing True (takeDirectory localClientJsonPath))

        lift (downloadFile localClientJsonPath clientJsonUrl) >>= \case
            Right () ->
                lift (putStrLn "OK")

            Left errMsg -> do
                lift (putStrLn "ERROR")
                error (printf "Failed to download ClientJson: %s" errMsg)

readClientJson :: HasCallStack => MCVersion -> AppStateT IO ClientJson
readClientJson mcVersion = do
    minecraftDir <- getMinecraftDir

    let localClientJsonPath = getLocalClientJsonPath minecraftDir mcVersion

    rawClientJson <- lift (BS.readFile localClientJsonPath)

    return (either error id (parseClientJson rawClientJson))

downloadClientJar :: HasCallStack => ClientJson -> AppStateT IO ()
downloadClientJar clientJson = do
    minecraftDir <- getMinecraftDir

    let clientJarUrl = getClientJarUrl clientJson
        localClientJarPath = getLocalClientJarPath minecraftDir clientJson

    unlessM (lift (doesFileExist localClientJarPath)) $ do
        lift (putStr "Downloading ClientJar ...")
        lift (hFlush stdout)

        lift (createDirectoryIfMissing True (takeDirectory localClientJarPath))

        lift (downloadFile localClientJarPath clientJarUrl) >>= \case
            Right () ->
                lift (putStrLn "OK")

            Left errMsg -> do
                lift (putStrLn "ERROR")
                error (printf "Failed to download ClientJar: %s" errMsg)

downloadAssetIndex :: HasCallStack => ClientJson -> AppStateT IO ()
downloadAssetIndex clientJson = do
    minecraftDir <- getMinecraftDir

    let assetIndexUrl = getAssetIndexUrl clientJson
        localAssetIndexPath = getLocalAssetIndexPath minecraftDir clientJson

    unlessM (lift (doesFileExist localAssetIndexPath)) $ do
        lift (putStr "Downloading AssetIndex ...")
        lift (hFlush stdout)

        lift (createDirectoryIfMissing True (takeDirectory localAssetIndexPath))

        lift (downloadFile localAssetIndexPath assetIndexUrl) >>= \case
            Right () ->
                lift (putStrLn "OK")

            Left errMsg -> do
                lift (putStrLn "ERROR")
                error (printf "Failed to download AssetIndex: %s" errMsg)


readAssetIndex :: HasCallStack => ClientJson -> AppStateT IO AssetIndex
readAssetIndex clientJson = do
    minecraftDir <- getMinecraftDir

    let localAssetIndexPath = getLocalAssetIndexPath minecraftDir clientJson

    rawAssetIndex <- lift (BS.readFile localAssetIndexPath)

    return (either error id (parseAssetIndex rawAssetIndex))

downloadAssets :: HasCallStack => ClientJson -> AssetIndex -> AppStateT IO ()
downloadAssets clientJson assetIndex = do
    minecraftDir <- getMinecraftDir

    let assetObjects = getAssetObjects assetIndex
        getLocalAssetObjectPath' = getLocalAssetObjectPath minecraftDir clientJson assetIndex

    assetObjectsToDownload <- flip filterM assetObjects $ \assetObj ->
        let localAssetObjectPath = getLocalAssetObjectPath' assetObj in
            lift (doesFileExist localAssetObjectPath) <&> not

    unless (null assetObjectsToDownload) $ do
        progressBar <- lift (newSimpleProgressBar "Downloading assets" (length assetObjectsToDownload))

        let assetChunks = chunksOf 50 assetObjectsToDownload

        lift $ forM_ assetChunks $ \assetChunk ->
            forConcurrently_ assetChunk $ \assetObj -> do
                let assetObjectUrl = getAssetObjectUrl assetObj
                    localAssetObjectPath = getLocalAssetObjectPath' assetObj

                createDirectoryIfMissing True (takeDirectory localAssetObjectPath)

                downloadFile localAssetObjectPath assetObjectUrl >>= \case
                    Right () ->
                        incProgress progressBar 1

                    Left errMsg ->
                        error (printf "Failed to download an asset object: %s" errMsg)

downloadLibraries :: HasCallStack => ClientJson -> AppStateT IO ()
downloadLibraries clientJson = do
    minecraftDir <- getMinecraftDir
    osVer <- getOSVersion

    let ruleContext =
            RuleContext
                { osVersion = osVer
                , isDemoUser = False
                , hasCustomResolution = False
                , hasQuickPlaysSupport = False
                , isQuickPlaySinglePlayer = False
                , isQuickPlayMultiplayer = False
                , isQuickPlayRealms = False
                }
        clientLibraries = getClientLibraries clientJson
        adoptedLibraries = filterLibraries ruleContext clientLibraries

    librariesToDownload <- flip filterM adoptedLibraries $ \lib ->
        let localLibraryPath = getLocalLibraryPath minecraftDir lib in
            lift (doesFileExist localLibraryPath) <&> not

    unless (null librariesToDownload) $ do
        progressBar <- lift (newSimpleProgressBar "Downloading libraries" (length librariesToDownload))

        lift $ forConcurrently_ librariesToDownload $ \lib -> do
            let libraryUrl = getLibraryArtifactUrl lib
                localLibraryPath = getLocalLibraryPath minecraftDir lib

            createDirectoryIfMissing True (takeDirectory localLibraryPath)

            downloadFile localLibraryPath libraryUrl >>= \case
                Right () ->
                    incProgress progressBar 1

                Left errMsg ->
                    error (printf "Failed to download a library: %s" errMsg)

prepareMinecraftLaunch :: HasCallStack => MCVersion -> AppStateT IO ()
prepareMinecraftLaunch mcVersion = do
    downloadClientJson mcVersion
    clientJson <- readClientJson mcVersion
    downloadClientJar clientJson

    downloadAssetIndex clientJson
    assetIndex <- readAssetIndex clientJson
    downloadAssets clientJson assetIndex

    downloadLibraries clientJson
