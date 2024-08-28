{-# OPTIONS_GHC -Wno-partial-fields #-}

module Interface.CommandPrompt.Command.QuickLaunchCommand (QuickLaunchCommand (QuickLaunchCommand)) where

import           Interface.CommandPrompt.Command           (Command (..))
import           Internal.AppState                         (AppStateT,
                                                            getVersionManifest)
import           Procedure.MinecraftLauncher.LaunchPrepare (prepareMinecraftLaunch)

import           Control.Monad.Trans.Class                 (lift)
import           Data.List                                 (find)
import           Data.Minecraft.VersionManifest
import           Options.Applicative
import           Text.Printf                               (printf)

data QuickLaunchCommand = QuickLaunchCommand
                        | QuickLaunchCommandOptions
                            { launchVersion_ :: String
                            }

instance Command QuickLaunchCommand where
    commandDesc = const "Launch a Minecraft client without making an installation."

    commandArgParser = const (Just quickLaunchCommandArgParser)

    commandProcedure = quickLaunchCommandProcedure

quickLaunchCommandArgParser :: AppStateT IO (Parser QuickLaunchCommand)
quickLaunchCommandArgParser = do
    versionManifest <- getVersionManifest

    return $
        QuickLaunchCommandOptions
            <$> strOption
                ( help "Specify a Minecraft client version."
               <> value (getLatestReleaseID versionManifest)
               <> showDefault
               <> metavar "MCVersion"
               <> long "version"
               <> short 'v'
                )

quickLaunchCommandProcedure :: QuickLaunchCommand -> AppStateT IO ()
quickLaunchCommandProcedure opts = do
    let launchVersion = launchVersion_ opts

    versionManifest <- getVersionManifest

    case find (\mcVer -> getMCVersionID mcVer == launchVersion) (getMCVersions versionManifest) of
        Just mcVersion -> do
            lift (putStrLn (printf "Launching Minecraft %s ..." launchVersion))

            prepareMinecraftLaunch mcVersion

        Nothing ->
            lift (putStrLn (printf "Version '%s' is unavailable." launchVersion))
