{-# OPTIONS_GHC -Wno-partial-fields #-}

module Procedure.CommandPrompt.Command.QuickLaunchCommand (QuickLaunchCommand (QuickLaunchCommand)) where

import           Interface.CommandPrompt.Command  (Command (..))
import           Internal.AppState                (AppStateT,
                                                   getVersionManifest)
import           Procedure.MinecraftLauncher      (LaunchContext (..),
                                                   launchMinecraft)

import           Control.Monad                    (unless)
import           Control.Monad.Trans.Class        (lift)
import           Data.List                        (find)
import           Data.Maybe                       (isJust, isNothing)
import           Data.Minecraft.VersionManifestV2
import           GHC.Stack                        (HasCallStack)
import           Options.Applicative
import           Text.Printf                      (printf)

data QuickLaunchCommand = QuickLaunchCommand
                        | QuickLaunchCommandOptions
                            { launchVersion_ :: String
                            , width_         :: Maybe Int
                            , height_        :: Maybe Int
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
            <*> optional
                ( option auto
                    ( help "Use a custom resolution."
                   <> metavar "Int"
                   <> long "width"
                   <> short 'w'
                    )
                )
            <*> optional
                ( option auto
                    ( help "Use a custom resolution."
                   <> metavar "Int"
                   <> long "height"
                   <> short 'h'
                    )
                )

quickLaunchCommandProcedure :: HasCallStack => QuickLaunchCommand -> AppStateT IO ()
quickLaunchCommandProcedure opts = do
    let launchVersion = launchVersion_ opts
        width = width_ opts
        height = height_ opts

    unless ((isJust width && isJust height) || (isNothing width && isNothing height)) $
        error "Please specify both '--width' and '--height' simultaneously."

    versionManifest <- getVersionManifest

    case find (\mcVer -> getMCVersionID mcVer == launchVersion) (getMCVersions versionManifest) of
        Just mcVersion -> do
            lift (putStrLn (printf "Launching Minecraft %s ..." launchVersion))

            let launchCtx =
                    LaunchContext
                        { windowWidth = width
                        , windowHeight = height
                        , shouldUseDemoMode = False
                        , quickPlaySinglePlayer = Nothing
                        , quickPlayMultiPlayer = Nothing
                        , quickPlayRealms = Nothing
                        }

            launchMinecraft mcVersion launchCtx

        Nothing ->
            error (printf "Version '%s' is unavailable." launchVersion)
