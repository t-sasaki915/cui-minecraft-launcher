{-# OPTIONS_GHC -Wno-partial-fields #-}
{-# LANGUAGE NoOverloadedStrings #-}

module Interface.CommandPrompt.Command.ListVersionCommand (ListVersionCommand (ListVersionCommand)) where

import           Interface.CommandPrompt.Command (Command (..))
import           Internal.AppState               (AppStateT, getVersionManifest)

import           Control.Monad.Trans.Class       (lift)
import           Data.Minecraft.VersionManifest
import           Data.Time                       (UTCTime, defaultTimeLocale,
                                                  formatTime)
import           Options.Applicative
import           Text.Layout.Table
import           Text.Printf                     (printf)

data ListVersionCommand = ListVersionCommand
                        | ListVersionCommandOptions
                            { showReleases_  :: Bool
                            , showSnapshots_ :: Bool
                            , showOldBetas_  :: Bool
                            , showOldAlphas_ :: Bool
                            }

instance Command ListVersionCommand where
    commandDesc = const "Show a list of available Minecraft versions."

    commandArgParser = const (Just listVersionCommandArgParser)

    commandProcedure = listVersionCommandProcedure

listVersionCommandArgParser :: AppStateT IO (Parser ListVersionCommand)
listVersionCommandArgParser =
    return $
        ListVersionCommandOptions
            <$> switch
                ( help "Show release versions."
               <> long "releases"
               <> short 'r'
                )
            <*> switch
                ( help "Show snapshot versions."
               <> long "snapshots"
               <> short 's'
                )
            <*> switch
                ( help "Show old_beta versions."
               <> long "oldbetas"
               <> short 'b'
                )
            <*> switch
                ( help "Show old_alpha versions."
               <> long "oldalphas"
               <> short 'a'
                )

listVersionCommandProcedure :: ListVersionCommand -> AppStateT IO ()
listVersionCommandProcedure opts = do
    let showReleases = showReleases_ opts
        showSnapshots = showSnapshots_ opts
        showOldBetas  = showOldBetas_ opts
        showOldAlphas = showOldAlphas_ opts

    versionManifest <- getVersionManifest
    let latestRelease = getLatestReleaseID versionManifest
        latestSnapshot = getLatestSnapshotID versionManifest
        isLatest vID = vID == latestRelease || vID == latestSnapshot

    let versionsToShow
            | not showReleases && not showSnapshots && not showOldBetas && not showOldAlphas =
                flip filter (getMCVersions versionManifest) $ \mcVersion ->
                    let vType = getMCVersionType mcVersion in
                        vType == Release

            | otherwise =
                flip filter (getMCVersions versionManifest) $ \mcVersion ->
                    let vType = getMCVersionType mcVersion in
                        (showReleases && vType == Release) ||
                        (showSnapshots && vType == Snapshot) ||
                        (showOldBetas && vType == OldBeta) ||
                        (showOldAlphas && vType == OldAlpha)

    let rgs = map rowG $
            flip map (reverse versionsToShow) $ \mcVersion ->
                let versionID = getMCVersionID mcVersion
                    versionType = getMCVersionType mcVersion
                    releaseDate = formatReleaseTime (getMCVersionReleaseTime mcVersion) in
                    [ show versionType
                    , if isLatest versionID then printf "%s (latest)" versionID else versionID
                    , releaseDate
                    ]

        table = columnHeaderTableS
            [defColSpec, defColSpec, defColSpec] asciiS (titlesH ["Type", "ID", "Release Date"]) rgs

    lift (putStrLn (tableString table))

formatReleaseTime :: UTCTime -> String
formatReleaseTime = formatTime defaultTimeLocale "%d %b %Y %Z"
