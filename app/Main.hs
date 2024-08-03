module Main (main) where

import           Imports

import           AppOption
import           AppState
import           REPL.REPLMain                  (replMain, replTabCompletion)

import           Control.Either.Extra           (throwEither)
import           Data.Minecraft.VersionManifest
import           Data.Version                   (showVersion)
import           Game.Minecraft.MinecraftFiles
import           Options.Applicative
import           System.Console.Haskeline
import           System.Directory
import           System.OperatingSystem         (currentOSType)

initialiseVersionManifest :: HasCallStack => AppStateT IO ()
initialiseVersionManifest = do
    minecraftDir <- getMinecraftGameDir
    let localVersionManifestPath = getVersionManifestPath minecraftDir

    lift fetchVersionManifestFromMojang >>= \case
        Right versionManifestJson -> do
            putStrLn' "Using the latest Minecraft version list."

            let versionManifest = throwEither (parseVersionManifest versionManifestJson)

            whenM (lift (doesFileExist localVersionManifestPath)) $
                lift (removeFile localVersionManifestPath)

            lift (writeFile localVersionManifestPath versionManifestJson)
            initialiseVersionManifestWith versionManifest

        Left errMsg -> do
            putStrLn' (printf "Failed to fetch Minecraft versions from Mojang server: %s" errMsg)

            unlessM (lift (doesFileExist localVersionManifestPath)) $
                error "Could not find the local VersionManifest. Please connect to the internet."

            putStrLn' "Using the local Minecraft version list."

            versionManifestJson <- lift (readFile localVersionManifestPath)
            let versionManifest = throwEither (parseVersionManifest versionManifestJson)

            initialiseVersionManifestWith versionManifest

main :: IO ()
main = do
    appOptionParser <- getAppOptionParser
    appOption <- customExecParser (prefs disambiguate)
        (info (helper <*> appOptionParser)
            (fullDesc <> header
                (printf "This is cui-minecraft-launcher %s (%s) by TSasaki."
                    (showVersion version) (show currentOSType))))

    void $ flip runAppStateT (initialAppState appOption) $ do
        minecraftDir <- getMinecraftGameDir
        lift (createMinecraftDirectoriesIfMissing minecraftDir)
        putStrLn' (printf "Using '%s' as the Minecraft game directory." minecraftDir)

        initialiseVersionManifest

        let
            haskelineSettings =
                Settings
                    { historyFile    = Nothing
                    , complete       = completeWord Nothing " \t" replTabCompletion
                    , autoAddHistory = True
                    }

        runInputT haskelineSettings replMain
