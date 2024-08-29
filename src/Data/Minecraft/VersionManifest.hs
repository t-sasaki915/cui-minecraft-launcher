module Data.Minecraft.VersionManifest
    ( MCVersionID
    , MCVersionType (..)
    , MCVersion
    , VersionManifest
    , getLatestReleaseID
    , getLatestSnapshotID
    , getMCVersions
    , getMCVersionID
    , getMCVersionType
    , getMCVersionReleaseTime
    , getClientJsonUrl
    , parseVersionManifest
    , getVersionManifestUrl
    , getLocalVersionManifestPath
    ) where

import           Data.Aeson      (FromJSON (parseJSON), Value (Object, String),
                                  eitherDecodeStrict', (.:))
import           Data.ByteString (ByteString)
import           Data.Minecraft  (MinecraftDir)
import           Data.Time.Clock (UTCTime)
import           System.FilePath ((</>))
import           Text.Printf     (printf)

type MCVersionID = String

data LatestVersions = LatestVersions
    { latestRelease_  :: MCVersionID
    , latestSnapshot_ :: MCVersionID
    }
    deriving Show

instance FromJSON LatestVersions where
    parseJSON (Object m) =
        LatestVersions
            <$> (m .: "release")
            <*> (m .: "snapshot")
    parseJSON x = fail (printf "Invalid LatestVersions structure: %s" (show x))

data MCVersionType = Release | Snapshot | OldBeta | OldAlpha deriving (Show, Eq)

instance FromJSON MCVersionType where
    parseJSON (String "release")   = pure Release
    parseJSON (String "snapshot")  = pure Snapshot
    parseJSON (String "old_beta")  = pure OldBeta
    parseJSON (String "old_alpha") = pure OldAlpha
    parseJSON x = fail (printf "Invalid MCVersionType structure: %s" (show x))

data MCVersion = MCVersion
    { mcVersionID_          :: MCVersionID
    , mcVersionType_        :: MCVersionType
    , mcVersionReleaseTime_ :: UTCTime
    , clientJsonUrl_        :: String
    }
    deriving Show

instance FromJSON MCVersion where
    parseJSON (Object m) =
        MCVersion
            <$> (m .: "id")
            <*> (m .: "type")
            <*> (m .: "releaseTime")
            <*> (m .: "url")
    parseJSON x = fail (printf "Invalid MCVersion structure: %s" (show x))

data VersionManifest = VersionManifest
    { latestVersions_ :: LatestVersions
    , mcVersions_     :: [MCVersion]
    }
    deriving Show

instance FromJSON VersionManifest where
    parseJSON (Object m) =
        VersionManifest
            <$> (m .: "latest")
            <*> (m .: "versions")
    parseJSON x = fail (printf "Invalid VersionManifest structure: %s" (show x))

getLatestReleaseID :: VersionManifest -> MCVersionID
getLatestReleaseID = latestRelease_ . latestVersions_

getLatestSnapshotID :: VersionManifest -> MCVersionID
getLatestSnapshotID = latestSnapshot_ . latestVersions_

getMCVersions :: VersionManifest -> [MCVersion]
getMCVersions = mcVersions_

getMCVersionID :: MCVersion -> MCVersionID
getMCVersionID = mcVersionID_

getMCVersionType :: MCVersion -> MCVersionType
getMCVersionType = mcVersionType_

getMCVersionReleaseTime :: MCVersion -> UTCTime
getMCVersionReleaseTime = mcVersionReleaseTime_

getClientJsonUrl :: MCVersion -> String
getClientJsonUrl = clientJsonUrl_

parseVersionManifest :: ByteString -> Either String VersionManifest
parseVersionManifest =
    either (Left . printf "Failed to parse VersionManifest: %s") Right .
        eitherDecodeStrict'

getVersionManifestUrl :: String
getVersionManifestUrl = "https://piston-meta.mojang.com/mc/game/version_manifest.json"

getLocalVersionManifestPath :: MinecraftDir -> FilePath
getLocalVersionManifestPath = (</> "versions" </> "version_manifest.json")
