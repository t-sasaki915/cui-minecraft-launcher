module Data.Minecraft.VersionManifest (MCVersion(..), VersionManifest(..)) where

import           Core.Data.Clock (Time)
import           Data.Aeson
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

data MCVersionType = Release | Snapshot deriving Show

instance FromJSON MCVersionType where
    parseJSON (String "release")  = pure Release
    parseJSON (String "snapshot") = pure Snapshot
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
