module Game.Minecraft.MinecraftLaunch (prepareMinecraftLaunch) where

import           Imports

import           AppState

import           Control.Either.Extra           (throwEither)
import           Data.Minecraft.ClientJson      (parseClientJson)
import           Data.Minecraft.VersionManifest
import           Game.Minecraft.MinecraftFiles
import           Network.Curl                   (curlExecutableName)
import           System.Directory               (createDirectoryIfMissing)
import           System.Exit                    (ExitCode (ExitSuccess))
import           System.Process                 (proc,
                                                 readCreateProcessWithExitCode)

createVersionDirectoryIfMissing :: MCVersionID -> AppStateT IO ()
createVersionDirectoryIfMissing versionID = do
    minecraftDir <- getMinecraftGameDir
    lift (createDirectoryIfMissing True (getMinecraftVersionDir versionID minecraftDir))

downloadAndReadClientJson :: HasCallStack => MCVersionID -> AppStateT IO String
downloadAndReadClientJson versionID = do
    availableVersions <- getVersionManifest <&> versions

    case lookup versionID (map (\(MCVersion vID _ vUrl _ _) -> (vID, vUrl)) availableVersions) of
        Just clientJsonUrl -> do
            localClientJsonPath <- getMinecraftGameDir <&> getClientJsonPath versionID

            lift (doesFileExist localClientJsonPath) >>= \case
                True -> do
                    lift (readFile localClientJsonPath)

                False -> do
                    putStr' "Downloading a client.json ... "

                    let
                        curlArgs =
                            [ "--fail"
                            , "--silent"
                            , "--show-error"
                            , clientJsonUrl
                            ]

                    (exitCode, stdout, stderr) <- lift (readCreateProcessWithExitCode
                        (proc curlExecutableName curlArgs) [])

                    case exitCode of
                        ExitSuccess -> do
                            putStrLn' "OK."
                            lift (writeFile localClientJsonPath stdout)
                            return stdout

                        _ -> do
                            putStrLn' "Error."
                            error (printf "Failed to download client.json: %s" stderr)

        Nothing ->
            error (printf "'%s' is not an available Minecraft version." versionID)

prepareMinecraftLaunch :: HasCallStack => MCVersionID -> AppStateT IO ()
prepareMinecraftLaunch versionID = do
    createVersionDirectoryIfMissing versionID

    rawClientJson <- downloadAndReadClientJson versionID
    let clientJson = throwEither (parseClientJson rawClientJson)
    --putStrLn' (show clientJson)
    return ()
