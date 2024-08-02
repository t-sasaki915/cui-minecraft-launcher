module Main (main) where

import           Imports

import           AppOption
import           AppState
import           CrossPlatform            (currentOSType)
import           REPL.REPLMain            (replMain)

import           Data.Version             (showVersion)
import           Options.Applicative
import           System.Console.Haskeline (defaultSettings, runInputT)
import           System.Directory         (createDirectoryIfMissing)

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
        lift $ do
            putStrLn (printf "Using '%s' as the Minecraft game directory." minecraftDir)
            createDirectoryIfMissing True minecraftDir

        runInputT defaultSettings replMain
