module Procedure.MinecraftLauncher.JavaRuntimePrepare (prepareJavaRuntime) where

import           Internal.AppState

import           Control.Concurrent.Async     (forConcurrently_)
import           Control.Monad                (forM_, unless, when)
import           Control.Monad.Extra          (unlessM)
import           Control.Monad.Trans.Class    (lift)
import           Crypto.Hash.Sha1.Extra       (stringHash)
import qualified Data.ByteString              as BS
import           Data.Functor                 ((<&>))
import           Data.JavaRuntime
import           Data.JavaRuntime.JreManifest
import           Data.List.Extra              (chunksOf)
import           GHC.Stack                    (HasCallStack)
import           Network.Curl                 (downloadFile)
import           System.Directory             (createDirectoryIfMissing,
                                               createFileLink, doesFileExist)
import           System.FilePath              (takeDirectory, (</>))
import           System.IO                    (hFlush, stdout)
import           System.OS                    (OSType (Windows), currentOSType)
import           System.Process.Extra2        (execProcessEither)
import           System.ProgressBar           (incProgress)
import           System.ProgressBar.Extra     (newSimpleProgressBar)
import           Text.Printf                  (printf)

determineJavaRuntimeManifest :: HasCallStack => String -> AppStateT IO JavaRuntimeManifest
determineJavaRuntimeManifest variant = do
    jreManifest <- getJreManifest
    case getJavaRuntimeManifest jreManifest variant of
        Just manifest -> return manifest
        Nothing       -> error (printf "%s is not available for this system." (show variant))

downloadJavaRuntimeManifest :: HasCallStack => JavaRuntimeManifest -> AppStateT IO ()
downloadJavaRuntimeManifest manifest = do
    minecraftDir <- getMinecraftDir

    let javaRuntimeId = getJavaRuntimeManifestId manifest
        javaRuntimeManifestUrl = getJavaRuntimeManifestUrl manifest
        javaRuntimeManifestSha1 = getJavaRuntimeManifestSha1 manifest
        localJavaRuntimeManifestPath = getLocalJavaRuntimeManifestPath minecraftDir javaRuntimeId

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

readJavaRuntimeManifest :: HasCallStack => JavaRuntimeManifest -> AppStateT IO JavaRuntime
readJavaRuntimeManifest manifest = do
    minecraftDir <- getMinecraftDir

    let javaRuntimeManifestId = getJavaRuntimeManifestId manifest
        localJavaRuntimeManifestPath = getLocalJavaRuntimeManifestPath minecraftDir javaRuntimeManifestId

    rawJavaRuntime <- lift (BS.readFile localJavaRuntimeManifestPath)

    return (either error id (parseJavaRuntimeManifest rawJavaRuntime))

downloadJavaRuntime :: HasCallStack => JavaRuntimeManifest -> JavaRuntime -> AppStateT IO ()
downloadJavaRuntime manifest runtime = do
    minecraftDir <- getMinecraftDir

    let javaRuntimeManifestId = getJavaRuntimeManifestId manifest
        runtimeFiles = getJavaRuntimeFiles runtime
        runtimeFileChunks = chunksOf 50 (filter ((/= Link) . getJavaRuntimeFileType) runtimeFiles)
        getLocalJavaRuntimeFilePath' = getLocalJavaRuntimeFilePath minecraftDir javaRuntimeManifestId

    progressBar <- lift (newSimpleProgressBar "Downloading JavaRuntime" (length runtimeFiles))

    lift $ forM_ runtimeFileChunks $ \runtimeFileChunk ->
        forConcurrently_ runtimeFileChunk $ \runtimeFile ->
            let localRuntimeFilePath = getLocalJavaRuntimeFilePath' runtimeFile in
                case getJavaRuntimeFileType runtimeFile of
                    Directory -> do
                        createDirectoryIfMissing True localRuntimeFilePath

                        incProgress progressBar 1

                    Link ->
                        return ()

                    File -> do
                        let runtimeFileUrl = getJavaRuntimeRawFileUrl runtimeFile
                            runtimeFileSha1 = getJavaRuntimeRawFileSha1 runtimeFile
                            runtimeFileName = getJavaRuntimeFileName runtimeFile
                            isRuntimeFileExecutable = isJavaRuntimeFileExecutable runtimeFile

                        fileExists <- doesFileExist localRuntimeFilePath
                        sha1Verification <- if fileExists
                            then BS.readFile localRuntimeFilePath <&> ((== runtimeFileSha1) . stringHash)
                            else return False

                        unless (not fileExists || not sha1Verification) $
                            incProgress progressBar 1

                        when (not fileExists || not sha1Verification) $ do
                            downloadFile localRuntimeFilePath runtimeFileUrl >>= \case
                                Right () ->
                                    return ()

                                Left errMsg ->
                                    error (printf "Failed to download a runtime file '%s': %s" runtimeFileName errMsg)

                            when (currentOSType /= Windows && isRuntimeFileExecutable) $
                                execProcessEither "chmod" ["+x", localRuntimeFilePath] >>= \case
                                    Right () ->
                                        return ()

                                    Left errMsg ->
                                        error (printf "Failed to make a file executable: %s" errMsg)

                            incProgress progressBar 1

    let runtimeFileLinks = filter ((== Link) . getJavaRuntimeFileType) (getJavaRuntimeFiles runtime)

    lift $ forConcurrently_ runtimeFileLinks $ \runtimeFileLink -> do
        let localRuntimeFilePath = getLocalJavaRuntimeFilePath' runtimeFileLink
            symlinkTarget = getJavaRuntimeFileSymlinkTarget runtimeFileLink
            absSymlinkTarget = takeDirectory localRuntimeFilePath </> symlinkTarget

        unlessM (doesFileExist localRuntimeFilePath) $
            createFileLink absSymlinkTarget localRuntimeFilePath

        incProgress progressBar 1

prepareJavaRuntime :: HasCallStack => String -> AppStateT IO ()
prepareJavaRuntime variant = do
    manifest <- determineJavaRuntimeManifest variant
    downloadJavaRuntimeManifest manifest

    javaRuntime <- readJavaRuntimeManifest manifest
    downloadJavaRuntime manifest javaRuntime
