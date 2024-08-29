module Procedure.MinecraftLauncher.LaunchPrepare (prepareMinecraftLaunch) where

import           Internal.AppState              (AppStateT, getMinecraftDir)

import           Control.Monad.Extra            (unlessM)
import           Control.Monad.Trans.Class      (lift)
import qualified Data.ByteString                as BS
import           Data.Minecraft.AssetIndex      (getLocalAssetIndexPath)
import           Data.Minecraft.ClientJson
import           Data.Minecraft.VersionManifest
import           GHC.Stack                      (HasCallStack)
import           Network.Curl                   (downloadFile)
import           System.Directory               (createDirectoryIfMissing,
                                                 doesFileExist)
import           System.FilePath                (takeDirectory)
import           System.IO                      (hFlush, stdout)
import           Text.Printf                    (printf)

downloadClientJsonIfMissing :: HasCallStack => MCVersion -> AppStateT IO ()
downloadClientJsonIfMissing mcVersion = do
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

downloadAssetIndexIfMissing :: HasCallStack => ClientJson -> AppStateT IO ()
downloadAssetIndexIfMissing clientJson = do
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


prepareMinecraftLaunch :: HasCallStack => MCVersion -> AppStateT IO ()
prepareMinecraftLaunch mcVersion = do
    downloadClientJsonIfMissing mcVersion
    clientJson <- readClientJson mcVersion

    downloadAssetIndexIfMissing clientJson

    return ()
