module Procedure.MinecraftLauncher.JavaRuntimePrepare (prepareJavaRuntime) where

import           Internal.AppState

import           Control.Concurrent.Async             (forConcurrently_)
import           Control.Monad                        (forM_, unless, when)
import           Control.Monad.Trans.Class            (lift)
import           Crypto.Hash.Sha1.Extra               (stringHash)
import qualified Data.ByteString                      as BS
import           Data.Functor                         ((<&>))
import           Data.JavaRuntime
import           Data.JavaRuntime.JavaRuntimeManifest
import           Data.List.Extra                      (chunksOf)
import           GHC.Stack                            (HasCallStack)
import           Network.Curl                         (downloadFile)
import           System.Directory                     (createDirectoryIfMissing,
                                                       doesFileExist)
import           System.FilePath                      (takeDirectory)
import           System.IO                            (hFlush, stdout)
import           System.ProgressBar                   (incProgress)
import           System.ProgressBar.Extra             (newSimpleProgressBar)
import           Text.Printf                          (printf)

determineJavaRuntimeManifest :: HasCallStack => JavaRuntimeVariant -> AppStateT IO JavaRuntimeManifest
determineJavaRuntimeManifest variant = do
    manifests <- getJavaRuntimeManifestAll
    case getJavaRuntimeManifest manifests variant of
        Just manifest -> return manifest
        Nothing       -> error (printf "%s is not available for this system." (show variant))

downloadJavaRuntimeManifest :: HasCallStack => JavaRuntimeVariant -> JavaRuntimeManifest -> AppStateT IO ()
downloadJavaRuntimeManifest variant manifest = do
    minecraftDir <- getMinecraftDir

    let javaRuntimeManifestUrl = getJavaRuntimeManifestUrl manifest
        javaRuntimeManifestSha1 = getJavaRuntimeManifestSha1 manifest
        localJavaRuntimeManifestPath = getLocalJavaRuntimeManifestPath minecraftDir variant

    fileExists <- lift (doesFileExist localJavaRuntimeManifestPath)
    sha1Verification <- if fileExists
        then lift (BS.readFile localJavaRuntimeManifestPath) <&> ((== javaRuntimeManifestSha1) . stringHash)
        else return False

    when (not fileExists || not sha1Verification) $ do
        lift (putStr "Downloading JavaRuntimeManifest ...")
        lift (hFlush stdout)

        lift (createDirectoryIfMissing True (takeDirectory localJavaRuntimeManifestPath))

        lift (downloadFile localJavaRuntimeManifestPath javaRuntimeManifestUrl) >>= \case
            Right () ->
                lift (putStrLn "OK")

            Left errMsg -> do
                lift (putStrLn "ERROR")
                error (printf "Failed to download JavaRuntimeManifest: %s" errMsg)

readJavaRuntimeManifest :: HasCallStack => JavaRuntimeVariant -> AppStateT IO JavaRuntime
readJavaRuntimeManifest variant = do
    minecraftDir <- getMinecraftDir

    let localJavaRuntimeManifestPath = getLocalJavaRuntimeManifestPath minecraftDir variant

    rawJavaRuntime <- lift (BS.readFile localJavaRuntimeManifestPath)

    return (either error id (parseJavaRuntimeManifest rawJavaRuntime))

downloadJavaRuntime :: HasCallStack => JavaRuntimeVariant -> JavaRuntime -> AppStateT IO ()
downloadJavaRuntime variant runtime = do
    minecraftDir <- getMinecraftDir

    let runtimeFiles = filter (not . isJavaRuntimeFileDirectory) (getJavaRuntimeFiles runtime)
        runtimeFileChunks = chunksOf 50 runtimeFiles
        getLocalJavaRuntimeFilePath' = getLocalJavaRuntimeFilePath minecraftDir variant

    progressBar <- lift (newSimpleProgressBar "Downloading JavaRuntime" (length runtimeFiles))

    lift $ forM_ runtimeFileChunks $ \runtimeFileChunk ->
        forConcurrently_ runtimeFileChunk $ \runtimeFile -> do
            let runtimeFileUrl = getJavaRuntimeRawFileUrl runtimeFile
                runtimeFileSha1 = getJavaRuntimeRawFileSha1 runtimeFile
                runtimeFileName = getJavaRuntimeFileName runtimeFile
                localRuntimeFilePath = getLocalJavaRuntimeFilePath' runtimeFile

            fileExists <- doesFileExist localRuntimeFilePath
            sha1Verification <- if fileExists
                then BS.readFile localRuntimeFilePath <&> ((== runtimeFileSha1) . stringHash)
                else return False

            unless (not fileExists || not sha1Verification) $
                incProgress progressBar 1

            when (not fileExists || not sha1Verification) $ do
                createDirectoryIfMissing True (takeDirectory localRuntimeFilePath)

                downloadFile localRuntimeFilePath runtimeFileUrl >>= \case
                    Right () ->
                        incProgress progressBar 1

                    Left errMsg ->
                        error (printf "Failed to download a runtime file '%s': %s" runtimeFileName errMsg)

prepareJavaRuntime :: HasCallStack => JavaRuntimeVariant -> AppStateT IO ()
prepareJavaRuntime variant = do
    manifest <- determineJavaRuntimeManifest variant
    downloadJavaRuntimeManifest variant manifest

    javaRuntime <- readJavaRuntimeManifest variant
    downloadJavaRuntime variant javaRuntime
