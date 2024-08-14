{-# OPTIONS_GHC -Wno-partial-fields #-}

module REPL.Command.ListVersionsCommand (ListVersionsCommand (ListVersionsCommand)) where

import           Imports

import           AppState

import           Data.Minecraft.VersionManifest
import           Data.Table
import           Data.Time
import           Options.Applicative
import           REPL.REPLCommand               (REPLCommand (..))


data ListVersionsCommand = ListVersionsCommand
                         | ListVersionsCommandOption
                            { showReleases  :: Bool
                            , showSnapshots :: Bool
                            , showOldBetas  :: Bool
                            , showOldAlphas :: Bool
                            }

instance REPLCommand ListVersionsCommand where
    commandDesc = const "Show the list of available Minecraft versions."

    commandArgParser = const (Just listVersionsCommandArgParser)

    commandProcedure = listVersionsCommandProcedure

listVersionsCommandArgParser :: AppStateT IO (Parser ListVersionsCommand)
listVersionsCommandArgParser =
    return $
        ListVersionsCommandOption
            <$> switch
                ( long "releases"
               <> short 'r'
               <> help "Show the list of Release Minecraft versions."
                )
            <*> switch
                ( long "snapshots"
               <> short 's'
               <> help "Show the list of Snapshot Minecraft versions."
                )
            <*> switch
                ( long "oldbetas"
               <> short 'b'
               <> help "Show the list of OldBeta Minecraft versions."
                )
            <*> switch
                ( long "oldalphas"
               <> short 'a'
               <> help "Show the list of OldAlpha Minecraft versions."
                )

listVersionsCommandProcedure :: HasCallStack => ListVersionsCommand -> AppStateT IO ()
listVersionsCommandProcedure opts = do
    let releases          = showReleases opts
        snapshots         = showSnapshots opts
        oldBetas          = showOldBetas opts
        oldAlphas         = showOldAlphas opts
        noOptionSpecified = not releases && not snapshots && not oldBetas && not oldAlphas

    mcVersions <- getVersionManifest <&> versions

    when noOptionSpecified $
        showVersionList mcVersions

    unless noOptionSpecified $ do
        let releasesToShow  = if releases  then filtVersionType Release  mcVersions else []
            snapshotsToShow = if snapshots then filtVersionType Snapshot mcVersions else []
            oldBetasToShow  = if oldBetas  then filtVersionType OldBeta  mcVersions else []
            oldAlphasToShow = if oldAlphas then filtVersionType OldAlpha mcVersions else []

        showVersionList (releasesToShow ++ snapshotsToShow ++ oldBetasToShow ++ oldAlphasToShow)

    where
        filtVersionType versionType = filter (\(MCVersion _ vType _ _ _) -> vType == versionType)

showVersionList :: [MCVersion] -> AppStateT IO ()
showVersionList mcVersions = do
    let table = createTable3 "Type" "ID" "Release Date" $
            addDataList $
                flip map mcVersions $ \mcv ->
                    ( mcVersionType mcv
                    , StringData (mcVersionID mcv)
                    , StringData (formatReleaseTime $ mcVersionReleaseTime mcv)
                    )

    putStrLn' (show table)

    where
        formatReleaseTime = formatTime defaultTimeLocale "%d %b %Y %Z"
