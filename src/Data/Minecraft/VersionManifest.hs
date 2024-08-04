module Data.Minecraft.VersionManifest
    ( MCVersionID
    , LatestVersions (..)
    , MCVersionType (..)
    , MCVersion (..)
    , VersionManifest (..)
    , fetchVersionManifestFromMojang
    , parseVersionManifest
    ) where

import           Data.Aeson
import           Data.ByteString (ByteString)
import           Data.Time.Clock (UTCTime)
import           Network.Curl    (readBSContentFromUrl)
import           Text.Printf     (printf)

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

data MCVersionType = Release | Snapshot | OldBeta | OldAlpha deriving (Show, Eq)

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
    , mcVersionTime        :: UTCTime
    , mcVersionReleaseTime :: UTCTime
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

fetchVersionManifestFromMojang :: IO (Either String ByteString)
fetchVersionManifestFromMojang =
    readBSContentFromUrl "https://piston-meta.mojang.com/mc/game/version_manifest_v2.json"

parseVersionManifest :: ByteString -> Either String VersionManifest
parseVersionManifest rawJson =
    case eitherDecodeStrict' rawJson of
        Right versionManifest ->
            Right versionManifest

        Left err ->
            Left (printf "Failed to parse version_manifest.json: %s" err)
