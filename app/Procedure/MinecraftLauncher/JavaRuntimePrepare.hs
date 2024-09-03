module Procedure.MinecraftLauncher.JavaRuntimePrepare (prepareJavaRuntime) where

import           Internal.AppState

import           Control.Monad                        (when)
import           Control.Monad.Trans.Class            (lift)
import           Crypto.Hash.Sha1.Extra               (stringHash)
import qualified Data.ByteString                      as BS
import           Data.Functor                         ((<&>))
import           Data.JavaRuntime.JavaRuntimeManifest
import           GHC.Stack                            (HasCallStack)
import           Network.Curl                         (downloadFile)
import           System.Directory                     (createDirectoryIfMissing,
                                                       doesFileExist)
import           System.FilePath                      (takeDirectory)
import           System.IO                            (hFlush, stdout)
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

prepareJavaRuntime :: HasCallStack => JavaRuntimeVariant -> AppStateT IO ()
prepareJavaRuntime variant = do
    manifest <- determineJavaRuntimeManifest variant

    downloadJavaRuntimeManifest variant manifest
