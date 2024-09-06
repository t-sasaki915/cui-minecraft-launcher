module Procedure.MinecraftLauncher.LaunchPrepare (prepareMinecraftLaunch) where

import           Internal.AppState                              (AppStateT,
                                                                 getMinecraftDir)
import           Procedure.MinecraftLauncher.JavaRuntimePrepare (prepareJavaRuntime)

import           Control.Concurrent.Async                       (forConcurrently_)
import           Control.Monad.Extra                            (forM_, unless,
                                                                 when)
import           Control.Monad.Trans.Class                      (lift)
import           Crypto.Hash.Sha1.Extra                         (stringHash)
import qualified Data.ByteString                                as BS
import           Data.Functor                                   ((<&>))
import           Data.List.Extra                                (chunksOf)
import           Data.Minecraft.AssetIndex
import           Data.Minecraft.ClientJson
import           Data.Minecraft.VersionManifestV2
import           GHC.Stack                                      (HasCallStack)
import           Network.Curl                                   (downloadFile)
import           System.Directory                               (createDirectoryIfMissing,
                                                                 doesFileExist)
import           System.FilePath                                (takeDirectory)
import           System.IO                                      (hFlush, stdout)
import           System.ProgressBar                             (incProgress)
import           System.ProgressBar.Extra                       (newSimpleProgressBar)
import           Text.Printf                                    (printf)

downloadClientJson :: HasCallStack => MCVersion -> AppStateT IO ()
downloadClientJson mcVersion = do
    minecraftDir <- getMinecraftDir

    let clientJsonUrl = getClientJsonUrl mcVersion
        clientJsonSha1 = getClientJsonSha1 mcVersion
        localClientJsonPath = getLocalClientJsonPath minecraftDir mcVersion

    fileExists <- lift (doesFileExist localClientJsonPath)
    sha1Verification <- if fileExists
        then lift (BS.readFile localClientJsonPath) <&> ((== clientJsonSha1) . stringHash)
        else return False

    when (not fileExists || not sha1Verification) $ do
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
        clientJarSha1 = getClientJarSha1 clientJson
        localClientJarPath = getLocalClientJarPath minecraftDir clientJson

    fileExists <- lift (doesFileExist localClientJarPath)
    sha1Verification <- if fileExists
        then lift (BS.readFile localClientJarPath) <&> ((== clientJarSha1) . stringHash)
        else return False

    when (not fileExists || not sha1Verification) $ do
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
        assetIndexSha1 = getAssetIndexSha1 clientJson
        localAssetIndexPath = getLocalAssetIndexPath minecraftDir clientJson

    fileExists <- lift (doesFileExist localAssetIndexPath)
    sha1Verification <- if fileExists
        then lift (BS.readFile localAssetIndexPath) <&> ((== assetIndexSha1) . stringHash)
        else return False

    when (not fileExists || not sha1Verification) $ do
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
        assetChunks = chunksOf 50 assetObjects
        getLocalAssetObjectPath' = getLocalAssetObjectPath minecraftDir clientJson assetIndex

    progressBar <- lift (newSimpleProgressBar "Downloading Assets" (length assetObjects))

    lift $ forM_ assetChunks $ \assetChunk ->
        forConcurrently_ assetChunk $ \assetObj -> do
            let assetObjUrl = getAssetObjectUrl assetObj
                assetObjSha1 = getAssetHash assetObj
                localAssetObjectPath = getLocalAssetObjectPath' assetObj

            fileExists <- doesFileExist localAssetObjectPath
            sha1Verification <- if fileExists
                then BS.readFile localAssetObjectPath <&> ((== assetObjSha1) . stringHash)
                else return False

            unless (not fileExists || not sha1Verification) $
                incProgress progressBar 1

            when (not fileExists || not sha1Verification) $ do
                createDirectoryIfMissing True (takeDirectory localAssetObjectPath)

                downloadFile localAssetObjectPath assetObjUrl >>= \case
                    Right () ->
                        incProgress progressBar 1

                    Left errMsg ->
                        error (printf "Failed to download an asset object '%s': %s" assetObjSha1 errMsg)

downloadLibraries :: HasCallStack => ClientJson -> RuleContext ->  AppStateT IO ()
downloadLibraries clientJson ruleContext = do
    minecraftDir <- getMinecraftDir

    let clientLibraries = getClientLibraries clientJson
        adoptedLibraries = filterLibraries ruleContext clientLibraries

    progressBar <- lift (newSimpleProgressBar "Downloading Libraries" (length adoptedLibraries))

    lift $ forConcurrently_ adoptedLibraries $ \lib -> do
        let libraryUrl = getLibraryArtifactUrl lib
            librarySha1 = getLibraryArtifactSha1 lib
            localLibraryPath = getLocalLibraryPath minecraftDir lib

        fileExists <- doesFileExist localLibraryPath
        sha1Verification <- if fileExists
            then BS.readFile localLibraryPath <&> ((== librarySha1) . stringHash)
            else return False

        unless (not fileExists || not sha1Verification) $
            incProgress progressBar 1

        when (not fileExists || not sha1Verification) $ do
            createDirectoryIfMissing True (takeDirectory localLibraryPath)

            downloadFile localLibraryPath libraryUrl >>= \case
                Right () ->
                    incProgress progressBar 1

                Left errMsg ->
                    error (printf "Failed to download a library '%s': %s" localLibraryPath errMsg)

prepareMinecraftLaunch :: HasCallStack => MCVersion -> RuleContext -> AppStateT IO ()
prepareMinecraftLaunch mcVersion ruleContext = do
    downloadClientJson mcVersion
    clientJson <- readClientJson mcVersion
    downloadClientJar clientJson
    downloadLibraries clientJson ruleContext

    downloadAssetIndex clientJson
    assetIndex <- readAssetIndex clientJson
    downloadAssets clientJson assetIndex

    prepareJavaRuntime (getJavaRuntimeVariant clientJson)
