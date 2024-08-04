module Game.Minecraft.MinecraftLaunch (prepareMinecraftLaunch) where

import           Imports

import           AppState

import           Control.Either.Extra           (throwEither)
import           Data.ByteString                (ByteString)
import qualified Data.ByteString                as BS
import           Data.Minecraft.ClientJson      (parseClientJson)
import           Data.Minecraft.VersionManifest
import           Game.Minecraft.MinecraftFiles
import           Network.Curl                   (readBSContentFromUrl)
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

                False ->
                    lift (readBSContentFromUrl clientJsonUrl) >>= \case
                        Right clientJsonRaw -> do
                            lift (BS.writeFile localClientJsonPath clientJsonRaw)
                            putStrLn' (printf "Downloaded a client.json for Minecraft %s." versionID)

                            return clientJsonRaw

                        Left errMsg ->
                            error (printf "Failed to download a client.json for Miecraft %s: %s" versionID errMsg)


        Nothing ->
            error (printf "'%s' is not an available Minecraft version." versionID)

prepareMinecraftLaunch :: HasCallStack => MCVersionID -> AppStateT IO ()
prepareMinecraftLaunch versionID = do
    createVersionDirectoryIfMissing versionID

    rawClientJson <- downloadAndReadClientJson versionID
    let clientJson = throwEither (parseClientJson rawClientJson)
    --putStrLn' (show clientJson)
    return ()
