module REPL.Command.FetchVersionsCommand (FetchVersionsCommand (FetchVersionsCommand)) where

import           Imports

import           AppOption
import           AppState
import           REPL.REPLCommand               (REPLCommand (..))

import           Data.Minecraft.VersionManifest (fetchVersionManifestFromMojang)
import           System.Directory               (removeFile)

data FetchVersionsCommand = FetchVersionsCommand

instance REPLCommand FetchVersionsCommand where
    commandDesc = const "Fetch Minecraft versions from Mojang server."

    commandArgParser = const Nothing

    commandProcedure = fetchVersionsCommandProcedure

fetchVersionsCommandProcedure :: HasCallStack => FetchVersionsCommand -> AppStateT IO ()
fetchVersionsCommandProcedure _ = do
    minecraftDir <- getAppState <&> (_minecraftGameDir . _appOption)
    let localVersionManifestPath = minecraftDir </> "versions" </> "version_manifest.json"

    whenM (lift (doesFileExist localVersionManifestPath)) $
        lift (removeFile localVersionManifestPath)

    putStrLn' "Fetching Minecraft versions from Mojang server..."

    versionManifest <- lift fetchVersionManifestFromMojang
    lift (writeFile localVersionManifestPath versionManifest)

    putStrLn' "Successfully fetched Minecraft versions."
