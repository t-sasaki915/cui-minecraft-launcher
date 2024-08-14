{-# OPTIONS_GHC -Wno-partial-fields #-}

module REPL.Command.QuickLaunchCommand (QuickLaunchCommand (QuickLaunchCommand)) where

import           Imports

import           AppState
import           Game.Minecraft.MinecraftLaunch (prepareMinecraftLaunch)
import           REPL.REPLCommand               (REPLCommand (..))

import           Data.Minecraft.VersionManifest
import           Options.Applicative

data QuickLaunchCommand = QuickLaunchCommand
                        | QuickLaunchCommandOptions
                            { launchClientVersion :: MCVersionID
                            }

instance REPLCommand QuickLaunchCommand where
    commandDesc = const "Launch a Minecraft client without making an installation."

    commandArgParser = const (Just quickLaunchCommandArgParser)

    commandProcedure = quickLaunchCommandProcedure

quickLaunchCommandArgParser :: AppStateT IO (Parser QuickLaunchCommand)
quickLaunchCommandArgParser = do
    versionManifest <- getVersionManifest
    let latestVer = latestRelease (latestVersions versionManifest)

    return $
        QuickLaunchCommandOptions
            <$> strOption
                ( long "version"
               <> short 'v'
               <> metavar "MCVersion"
               <> value latestVer
               <> showDefault
               <> help "Specify a Minecraft client version."
                )

quickLaunchCommandProcedure :: HasCallStack => QuickLaunchCommand -> AppStateT IO ()
quickLaunchCommandProcedure opts = do
    let clientVersion = launchClientVersion opts

    prepareMinecraftLaunch clientVersion
