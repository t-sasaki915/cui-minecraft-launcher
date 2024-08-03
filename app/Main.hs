module Main (main) where

import           Imports

import           AppOption
import           AppState
import           CrossPlatform                  (currentOSType)
import           REPL.REPLMain                  (replMain, replTabCompletion)

import           Data.Minecraft.VersionManifest (fetchVersionManifestFromMojang)
import           Data.Version                   (showVersion)
import           Options.Applicative
import           System.Console.Haskeline
import           System.Directory

main :: IO ()
main = do
    appOptionParser <- getAppOptionParser
    appOption <- customExecParser (prefs disambiguate)
        (info (helper <*> appOptionParser)
            (fullDesc <> header
                (printf "This is cui-minecraft-launcher %s (%s) by TSasaki."
                    (showVersion version) (show currentOSType))))

    void $ flip runAppStateT (initialAppState appOption) $ do
        minecraftDir <- getAppState <&> (_minecraftGameDir . _appOption)

        putStrLn' (printf "Using '%s' as the Minecraft game directory." minecraftDir)
        lift (createDirectoryIfMissing True minecraftDir)

        let minecraftVersionsDir = minecraftDir </> "versions"
            localVersionManifestPath = minecraftVersionsDir </> "version_manifest.json"
        lift (createDirectoryIfMissing True minecraftVersionsDir)

        whenM (lift (doesFileExist localVersionManifestPath)) $
            lift (removeFile localVersionManifestPath)

        putStrLn' "Fetching Minecraft versions from Mojang server..."

        versionManifest <- lift fetchVersionManifestFromMojang
        lift (writeFile localVersionManifestPath versionManifest)

        let
            haskelineSettings =
                Settings
                    { historyFile    = Nothing
                    , complete       = completeWord Nothing " \t" replTabCompletion
                    , autoAddHistory = True
                    }

        runInputT haskelineSettings replMain
