{-# OPTIONS_GHC -Wno-partial-fields #-}

module REPL.Command.ListVersionsCommand (ListVersionsCommand (ListVersionsCommand)) where

import           Imports

import           AppOption
import           AppState
import           REPL.Command.FetchVersionsCommand

import           Data.Minecraft.VersionManifest
import           Data.Time
import           Options.Applicative
import           REPL.REPLCommand                  (REPLCommand (..))


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
    minecraftDir <- getAppState <&> (_minecraftGameDir . _appOption)

    let releases          = showReleases opts
        snapshots         = showSnapshots opts
        oldBetas          = showOldBetas opts
        oldAlphas         = showOldAlphas opts
        noOptionSpecified = not releases && not snapshots && not oldBetas && not oldAlphas

        localVersionManifestPath = minecraftDir </> "versions" </> "version_manifest.json"

    unlessM (lift (doesFileExist localVersionManifestPath)) $
        internalExecuteREPLCommand FetchVersionsCommand []

    versionManifestJson <- lift (readFile localVersionManifestPath)
    let versionManifest = parseVersionManifest versionManifestJson
        mcVersions = versions versionManifest

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
    let maxVersionTypeLength        = maximum $ map (length . show . mcVersionType) mcVersions
        maxVersionIDLength          = maximum $ map (length . mcVersionID) mcVersions
        maxVersionReleaseDateLength = maximum $ map (length . formatReleaseTime . mcVersionReleaseTime) mcVersions

        formatter = printf "| %%-%ds | %%-%ds | %%-%ds |"
            maxVersionTypeLength maxVersionIDLength maxVersionReleaseDateLength

    putStrLn' (printf formatter ("Type" :: String) ("ID" :: String) ("Release Date" :: String))

    putStrLn' (printf "| %s | %s | %s |" (replicate maxVersionTypeLength '-')
        (replicate maxVersionIDLength '-') (replicate maxVersionReleaseDateLength '-'))

    forM_ mcVersions $ \(MCVersion vID vType _ _ vReleaseTime) ->
        putStrLn' (printf formatter (show vType) vID (formatReleaseTime vReleaseTime))

    where
        formatReleaseTime = formatTime defaultTimeLocale "%d %b %Y %Z"
