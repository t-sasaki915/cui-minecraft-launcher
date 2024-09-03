module Data.JavaRuntime.JavaRuntimeManifest
    ( JavaRuntimeVariant (..)
    , JavaRuntimeManifest
    , JavaRuntimeManifestAll
    , getJavaRuntimeManifestAllUrl
    , getLocalJavaRuntimeManifestAllPath
    , parseJavaRuntimeManifestAll
    , getJavaRuntimeManifest
    , getJavaRuntimeManifestSha1
    , getJavaRuntimeManifestUrl
    ) where

import           Data.Aeson
import           Data.ByteString (ByteString)
import           Data.Maybe      (listToMaybe)
import           Data.Minecraft  (MinecraftDir)
import           System.FilePath ((</>))
import           System.OS       (OSType (..), currentOSType)
import           System.OS.Arch  (OSArch (..), currentOSArch)
import           Text.Printf     (printf)

data JavaRuntimeVariant = JavaRuntimeAlpha
                        | JavaRuntimeBeta
                        | JavaRuntimeDelta
                        | JavaRuntimeGamma
                        | JavaRuntimeGammaSnapshot
                        | JreLegacy
                        | MinecraftJavaExe
                        deriving (Show, Eq)

instance FromJSON JavaRuntimeVariant where
    parseJSON (String "java-runtime-alpha")          = pure JavaRuntimeAlpha
    parseJSON (String "java-runtime-beta")           = pure JavaRuntimeBeta
    parseJSON (String "java-runtime-delta")          = pure JavaRuntimeDelta
    parseJSON (String "java-runtime-gamma")          = pure JavaRuntimeGamma
    parseJSON (String "java-runtime-gamma-snapshot") = pure JavaRuntimeGammaSnapshot
    parseJSON (String "jre-legacy")                  = pure JreLegacy
    parseJSON (String "minecraft-java-exe")          = pure MinecraftJavaExe
    parseJSON x = fail (printf "Invalid JavaRuntimeVariant structure: %s" (show x))

data JavaRuntimeManifestManifest = JavaRuntimeManifestManifest
    { javaRuntimeManifestSha1_ :: String
    , javaRuntimeManifestUrl_  :: String
    }
    deriving Show

instance FromJSON JavaRuntimeManifestManifest where
    parseJSON (Object m) =
        JavaRuntimeManifestManifest
            <$> (m .: "sha1")
            <*> (m .: "url")
    parseJSON x = fail (printf "Invalid JavaRuntimeManifestManifest structure: %s" (show x))

newtype JavaRuntimeManifest = JavaRuntimeManifest
    { javaRuntimeManifestManifest_ :: JavaRuntimeManifestManifest
    }
    deriving Show

instance FromJSON JavaRuntimeManifest where
    parseJSON (Object m) =
        JavaRuntimeManifest
            <$> (m .: "manifest")
    parseJSON x = fail (printf "Invalid JavaRuntimeManifest structure: %s" (show x))

data JavaRuntimeManifests = JavaRuntimeManifests
    { javaRuntimeAlphaManifests_         :: [JavaRuntimeManifest]
    , javaRuntimeBetaManifests_          :: [JavaRuntimeManifest]
    , javaRuntimeDeltaManifests_         :: [JavaRuntimeManifest]
    , javaRuntimeGammaManifests_         :: [JavaRuntimeManifest]
    , javaRuntimeGammaSnapshotManifests_ :: [JavaRuntimeManifest]
    , jreLegacyManifests_                :: [JavaRuntimeManifest]
    , minecraftJavaExeManifests_         :: [JavaRuntimeManifest]
    }
    deriving Show

instance FromJSON JavaRuntimeManifests where
    parseJSON (Object m) =
        JavaRuntimeManifests
            <$> (m .: "java-runtime-alpha")
            <*> (m .: "java-runtime-beta")
            <*> (m .: "java-runtime-delta")
            <*> (m .: "java-runtime-gamma")
            <*> (m .: "java-runtime-gamma-snapshot")
            <*> (m .: "jre-legacy")
            <*> (m .: "minecraft-java-exe")
    parseJSON x = fail (printf "Invalid JavaRuntimeManifests structure: %s" (show x))

data JavaRuntimeManifestAll = JavaRuntimeManifestAll
    { linuxManifests_        :: JavaRuntimeManifests
    , linuxI386Manifests_    :: JavaRuntimeManifests
    , macOSManifests_        :: JavaRuntimeManifests
    , macOSArm64Manifests_   :: JavaRuntimeManifests
    , windowsArm64Manifests_ :: JavaRuntimeManifests
    , windowsX64Manifests_   :: JavaRuntimeManifests
    , windowsX86Manifests_   :: JavaRuntimeManifests
    }
    deriving Show

instance FromJSON JavaRuntimeManifestAll where
    parseJSON (Object m) =
        JavaRuntimeManifestAll
            <$> (m .: "linux")
            <*> (m .: "linux-i386")
            <*> (m .: "mac-os")
            <*> (m .: "mac-os-arm64")
            <*> (m .: "windows-arm64")
            <*> (m .: "windows-x64")
            <*> (m .: "windows-x86")
    parseJSON x = fail (printf "Invalid JavaRuntimeManifestAll structure: %s" (show x))

getJavaRuntimeManifestAllUrl :: String
getJavaRuntimeManifestAllUrl =
    "https://piston-meta.mojang.com/v1/products/java-runtime/2ec0cc96c44e5a76b9c8b7c39df7210883d12871/all.json"

getLocalJavaRuntimeManifestAllPath :: MinecraftDir -> FilePath
getLocalJavaRuntimeManifestAllPath mcDir =
    mcDir </> "runtime" </> "all.json"

parseJavaRuntimeManifestAll :: ByteString -> Either String JavaRuntimeManifestAll
parseJavaRuntimeManifestAll =
    either (Left . printf "Failed to parse ClientJson: %s") Right .
        eitherDecodeStrict'

getJavaRuntimeManifests :: JavaRuntimeManifestAll -> JavaRuntimeManifests
getJavaRuntimeManifests manifestAll =
    case currentOSType of
        Windows ->
            case currentOSArch of
                Arm64  -> windowsArm64Manifests_ manifestAll
                X86_64 -> windowsX64Manifests_ manifestAll
                X86    -> windowsX86Manifests_ manifestAll
        OSX ->
            case currentOSArch of
                Arm64 -> macOSArm64Manifests_ manifestAll
                _     -> macOSManifests_ manifestAll
        Linux ->
            case currentOSArch of
                X86 -> linuxI386Manifests_ manifestAll
                _   -> linuxManifests_ manifestAll

getJavaRuntimeManifest :: JavaRuntimeManifestAll -> JavaRuntimeVariant -> Maybe JavaRuntimeManifest
getJavaRuntimeManifest manifestAll variant =
    let manifests = getJavaRuntimeManifests manifestAll in
        listToMaybe $ case variant of
            JavaRuntimeAlpha         -> javaRuntimeAlphaManifests_ manifests
            JavaRuntimeBeta          -> javaRuntimeBetaManifests_ manifests
            JavaRuntimeDelta         -> javaRuntimeDeltaManifests_ manifests
            JavaRuntimeGamma         -> javaRuntimeGammaManifests_ manifests
            JavaRuntimeGammaSnapshot -> javaRuntimeGammaSnapshotManifests_ manifests
            JreLegacy                -> jreLegacyManifests_ manifests
            MinecraftJavaExe         -> minecraftJavaExeManifests_ manifests

getJavaRuntimeManifestSha1 :: JavaRuntimeManifest -> String
getJavaRuntimeManifestSha1 = javaRuntimeManifestSha1_ . javaRuntimeManifestManifest_

getJavaRuntimeManifestUrl :: JavaRuntimeManifest -> String
getJavaRuntimeManifestUrl = javaRuntimeManifestUrl_ . javaRuntimeManifestManifest_
