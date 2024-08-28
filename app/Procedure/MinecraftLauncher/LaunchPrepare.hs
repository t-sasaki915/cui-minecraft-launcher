module Procedure.MinecraftLauncher.LaunchPrepare (prepareMinecraftLaunch) where

import           Internal.AppState              (AppStateT, getMinecraftDir)

import           Control.Monad.Extra            (unlessM)
import           Control.Monad.Trans.Class      (lift)
import           Data.Minecraft.ClientJson      (getLocalClientJsonPath)
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
        localClientJsonUrl = getLocalClientJsonPath minecraftDir mcVersion

    unlessM (lift (doesFileExist localClientJsonUrl)) $ do
        lift (putStr "Downloading client.json ...")
        lift (hFlush stdout)

        lift (createDirectoryIfMissing True (takeDirectory localClientJsonUrl))

        lift (downloadFile localClientJsonUrl clientJsonUrl) >>= \case
            Right () ->
                lift (putStrLn "OK")

            Left errMsg -> do
                lift (putStrLn "ERROR")
                error (printf "Failed to download client.json: %s" errMsg)

prepareMinecraftLaunch :: HasCallStack => MCVersion -> AppStateT IO ()
prepareMinecraftLaunch mcVersion = do
    downloadClientJsonIfMissing mcVersion
