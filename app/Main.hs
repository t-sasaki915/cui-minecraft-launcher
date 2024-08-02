module Main (main) where

import           Imports

import           AppOption
import           AppState
import           CrossPlatform            (currentOSType)
import           REPL.REPLMain            (replMain, replTabCompletion)

import           Data.Version             (showVersion)
import           Options.Applicative
import           System.Console.Haskeline
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

        putStrLn' (printf "Using '%s' as the Minecraft game directory." minecraftDir)
        lift (createDirectoryIfMissing True minecraftDir)

        let
            haskelineSettings =
                Settings
                    { historyFile    = Nothing
                    , complete       = completeWord Nothing " \t" replTabCompletion
                    , autoAddHistory = True
                    }

        runInputT haskelineSettings replMain
