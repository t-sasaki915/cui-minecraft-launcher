module Data.Minecraft.VersionManifestV2
    ( MCVersionID
    , MCVersionType (..)
    , MCVersion
    , VersionManifestV2
    , getLatestReleaseID
    , getLatestSnapshotID
    , getMCVersions
    , getMCVersionID
    , getMCVersionType
    , getMCVersionReleaseTime
    , getClientJsonUrl
    , parseVersionManifestV2
    , getVersionManifestV2Url
    , getLocalVersionManifestV2Path
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
    , clientJsonSha1_       :: String
    }
    deriving Show

instance FromJSON MCVersion where
    parseJSON (Object m) =
        MCVersion
            <$> (m .: "id")
            <*> (m .: "type")
            <*> (m .: "releaseTime")
            <*> (m .: "url")
            <*> (m .: "sha1")
    parseJSON x = fail (printf "Invalid MCVersion structure: %s" (show x))

data VersionManifestV2 = VersionManifestV2
    { latestVersions_ :: LatestVersions
    , mcVersions_     :: [MCVersion]
    }
    deriving Show

instance FromJSON VersionManifestV2 where
    parseJSON (Object m) =
        VersionManifestV2
            <$> (m .: "latest")
            <*> (m .: "versions")
    parseJSON x = fail (printf "Invalid VersionManifest structure: %s" (show x))

getLatestReleaseID :: VersionManifestV2 -> MCVersionID
getLatestReleaseID = latestRelease_ . latestVersions_

getLatestSnapshotID :: VersionManifestV2 -> MCVersionID
getLatestSnapshotID = latestSnapshot_ . latestVersions_

getMCVersions :: VersionManifestV2 -> [MCVersion]
getMCVersions = mcVersions_

getMCVersionID :: MCVersion -> MCVersionID
getMCVersionID = mcVersionID_

getMCVersionType :: MCVersion -> MCVersionType
getMCVersionType = mcVersionType_

getMCVersionReleaseTime :: MCVersion -> UTCTime
getMCVersionReleaseTime = mcVersionReleaseTime_

getClientJsonUrl :: MCVersion -> String
getClientJsonUrl = clientJsonUrl_

parseVersionManifestV2 :: ByteString -> Either String VersionManifestV2
parseVersionManifestV2 =
    either (Left . printf "Failed to parse VersionManifestV2: %s") Right .
        eitherDecodeStrict'

getVersionManifestV2Url :: String
getVersionManifestV2Url = "https://piston-meta.mojang.com/mc/game/version_manifest_v2.json"

getLocalVersionManifestV2Path :: MinecraftDir -> FilePath
getLocalVersionManifestV2Path = (</> "versions" </> "version_manifest_v2.json")
