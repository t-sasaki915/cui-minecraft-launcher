module Main (main) where

import           Imports

import           AppOption
import           AppState
import           REPL.REPLMain                  (replMain, replTabCompletion)

import           Control.Either.Extra           (throwEither, throwEitherM)
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

    whenM (lift (doesFileExist localVersionManifestPath)) $
        lift (removeFile localVersionManifestPath)

    putStrLn' "Fetching Minecraft versions from Mojang server..."

    versionManifestJson <- throwEitherM (lift fetchVersionManifestFromMojang)
    let versionManifest = throwEither (parseVersionManifest versionManifestJson)

    lift (writeFile localVersionManifestPath versionManifestJson)

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
