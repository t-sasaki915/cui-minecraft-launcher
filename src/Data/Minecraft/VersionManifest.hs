{-# LANGUAGE CPP #-}

module Data.Minecraft.VersionManifest
    ( MCVersionID
    , LatestVersions (..)
    , MCVersionType (..)
    , MCVersion (..)
    , VersionManifest (..)
    , fetchVersionManifestFromMojang
    , parseVersionManifest
    ) where

import           Control.Monad            (when)
import           Core.Data.Clock          (Time)
import           Data.Aeson
import           Data.ByteString          (pack)
import           Data.ByteString.Internal (c2w)
import           GHC.Stack                (HasCallStack)
import           System.Exit              (ExitCode (..))
import           System.Process
import           Text.Printf              (printf)

type MCVersionID = String

data LatestVersions = LatestVersions
    { latestRelease  :: MCVersionID
    , latestSnapshot :: MCVersionID
    }
    deriving Show

instance FromJSON LatestVersions where
    parseJSON (Object m) =
        LatestVersions
            <$> (m .: "release")
            <*> (m .: "snapshot")
    parseJSON x = fail (printf "Invalid LatestVersions structure: %s" (show x))

data MCVersionType = Release | Snapshot | OldBeta | OldAlpha deriving Show

instance FromJSON MCVersionType where
    parseJSON (String "release")   = pure Release
    parseJSON (String "snapshot")  = pure Snapshot
    parseJSON (String "old_beta")  = pure OldBeta
    parseJSON (String "old_alpha") = pure OldAlpha
    parseJSON x = fail (printf "Invalid MCVersionType structure: %s" (show x))

data MCVersion = MCVersion
    { mcVersionID          :: MCVersionID
    , mcVersionType        :: MCVersionType
    , mcVersionUrl         :: String
    , mcVersionTime        :: Time
    , mcVersionReleaseTime :: Time
    }
    deriving Show

instance FromJSON MCVersion where
    parseJSON (Object m) =
        MCVersion
            <$> (m .: "id")
            <*> (m .: "type")
            <*> (m .: "url")
            <*> (m .: "time")
            <*> (m .: "releaseTime")
    parseJSON x = fail (printf "Invalid MCVersion structure: %s" (show x))

data VersionManifest = VersionManifest
    { latestVersions :: LatestVersions
    , versions       :: [MCVersion]
    }
    deriving Show

instance FromJSON VersionManifest where
    parseJSON (Object m) =
        VersionManifest
            <$> (m .: "latest")
            <*> (m .: "versions")
    parseJSON x = fail (printf "Invalid VersionManifest structure: %s" (show x))

curlExecName :: String
#ifdef mingw32_HOST_OS
curlExecName = "curl.exe"
#endif
#ifdef linux_HOST_OS
curlExecName = "curl"
#endif
#ifdef darwin_HOST_OS
curlExecName = "curl"
#endif

fetchVersionManifestFromMojang :: HasCallStack => IO String
fetchVersionManifestFromMojang = do
    let
        curlArgs =
            [ "--fail"
            , "--silent"
            , "--show-error"
            , "https://launchermeta.mojang.com/mc/game/version_manifest.json"
            ]

    (exitCode, stdout, stderr) <- readCreateProcessWithExitCode
        (proc curlExecName curlArgs) []

    when (exitCode /= ExitSuccess) $
        error (printf "Failed to fetch version_manifest.json from Mojang server:\n%s" stderr)

    return stdout

parseVersionManifest :: HasCallStack => String -> VersionManifest
parseVersionManifest rawJson =
    case eitherDecodeStrict' (pack $ map c2w rawJson) of
        Right versionManifest ->
            versionManifest

        Left err ->
            error (printf "Failed to parse version_manifest.json: %s" err)
