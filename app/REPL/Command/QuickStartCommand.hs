{-# OPTIONS_GHC -Wno-partial-fields #-}

module REPL.Command.QuickStartCommand (QuickStartCommand (QuickStartCommand)) where

import           Imports

import           AppState
import           Game.Minecraft.MinecraftLaunch (prepareMinecraftLaunch)
import           REPL.REPLCommand               (REPLCommand (..))

import           Data.Minecraft.VersionManifest
import           Options.Applicative

data QuickStartCommand = QuickStartCommand
                       | QuickStartCommandOptions
                            { launchClientVersion :: MCVersionID
                            }

instance REPLCommand QuickStartCommand where
    commandDesc = const "Launch a Minecraft client without making an installation."

    commandArgParser = const (Just quickStartCommandArgParser)

    commandProcedure = quickStartCommandProcedure

quickStartCommandArgParser :: AppStateT IO (Parser QuickStartCommand)
quickStartCommandArgParser = do
    versionManifest <- getVersionManifest
    let latestVer = latestRelease (latestVersions versionManifest)

    return $
        QuickStartCommandOptions
            <$> strOption
                ( long "version"
               <> short 'v'
               <> metavar "MCVersion"
               <> value latestVer
               <> showDefault
               <> help "Specify a Minecraft client version."
                )

quickStartCommandProcedure :: HasCallStack => QuickStartCommand -> AppStateT IO ()
quickStartCommandProcedure opts = do
    let clientVersion = launchClientVersion opts

    prepareMinecraftLaunch clientVersion
