module Data.JavaRuntime.JreManifest
    ( JreManifest
    , JavaRuntimeManifest
    , getJreManifestUrl
    , getLocalJreManifestPath
    , getLocalJavaRuntimeManifestPath
    , parseJreManifest
    , getJavaRuntimeManifestId
    , getJavaRuntimeManifestSha1
    , getJavaRuntimeManifestUrl
    , getJavaRuntimeManifest
    ) where

import           Control.Monad.Extra        (concatForM)
import           Control.Monad.Trans.Except (Except, except, runExcept)
import           Data.Aeson
import           Data.Aeson.Key             (toString)
import qualified Data.Aeson.KeyMap          as KM
import           Data.Aeson.Types           (parseEither)
import           Data.ByteString            (ByteString)
import           Data.Functor               ((<&>))
import           Data.List                  (find)
import           Data.Maybe                 (listToMaybe)
import           Data.Minecraft             (MinecraftDir)
import           System.FilePath            ((</>))
import           System.OS                  (OSType (..), currentOSType)
import           System.OS.Arch             (OSArch (..), currentOSArch)
import           Text.Printf                (printf)

data JavaRuntimeManifest = JavaRuntimeManifest
    { javaRuntimeManifestId_   :: String
    , javaRuntimeManifestSha1_ :: String
    , javaRuntimeManifestUrl_  :: String
    }

data JavaRuntimeManifest_ = JavaRuntimeManifest_
    { javaRuntimeManifestSha1__ :: String
    , javaRuntimeManifestUrl__  :: String
    }
    deriving Show

instance FromJSON JavaRuntimeManifest_ where
    parseJSON (Object m) =
        JavaRuntimeManifest_
            <$> (m .: "sha1")
            <*> (m .: "url")
    parseJSON x = fail (printf "Invalid JavaRuntimeManifest structure: %s" (show x))

newtype JavaRuntimeManifest__ = JavaRuntimeManifest__
    { javaRuntimeManifest_ :: JavaRuntimeManifest_
    }
    deriving Show

instance FromJSON JavaRuntimeManifest__ where
    parseJSON (Object m) =
        JavaRuntimeManifest__
            <$> (m .: "manifest")
    parseJSON x = fail (printf "Invalid JavaRuntimeManifest structure: %s" (show x))

data JreManifest = JreManifest
    { linuxManifests_        :: [JavaRuntimeManifest]
    , linuxI386Manifests_    :: [JavaRuntimeManifest]
    , macOSManifests_        :: [JavaRuntimeManifest]
    , macOSArm64Manifests_   :: [JavaRuntimeManifest]
    , windowsArm64Manifests_ :: [JavaRuntimeManifest]
    , windowsX64Manifests_   :: [JavaRuntimeManifest]
    , windowsX86Manifests_   :: [JavaRuntimeManifest]
    }

data JreManifest_ = JreManifest_
    { linuxManifests__        :: Object
    , linuxI386Manifests__    :: Object
    , macOSManifests__        :: Object
    , macOSArm64Manifests__   :: Object
    , windowsArm64Manifests__ :: Object
    , windowsX64Manifests__   :: Object
    , windowsX86Manifests__   :: Object
    }
    deriving Show

instance FromJSON JreManifest_ where
    parseJSON (Object m) =
        JreManifest_
            <$> (m .: "linux")
            <*> (m .: "linux-i386")
            <*> (m .: "mac-os")
            <*> (m .: "mac-os-arm64")
            <*> (m .: "windows-arm64")
            <*> (m .: "windows-x64")
            <*> (m .: "windows-x86")
    parseJSON x = fail (printf "Invalid JreManifest structure: %s" (show x))

getJreManifestUrl :: String
getJreManifestUrl =
    "https://piston-meta.mojang.com/v1/products/java-runtime/2ec0cc96c44e5a76b9c8b7c39df7210883d12871/all.json"

getLocalJreManifestPath :: MinecraftDir -> FilePath
getLocalJreManifestPath mcDir =
    mcDir </> "versions" </> "jre_manifest.json"

getLocalJavaRuntimeManifestPath :: MinecraftDir -> String -> FilePath
getLocalJavaRuntimeManifestPath mcDir variant =
    mcDir </> "runtime" </> printf "%s.json" variant


parseJreManifest :: ByteString -> Either String JreManifest
parseJreManifest rawJson =
    case eitherDecodeStrict' rawJson of
        Right jreManifest_ -> runExcept $ do
            linuxManifests        <- parse (linuxManifests__ jreManifest_)
            linuxI386Manifests    <- parse (linuxI386Manifests__ jreManifest_)
            macOSManifests        <- parse (macOSManifests__ jreManifest_)
            macOSArm64Manifests   <- parse (macOSArm64Manifests__ jreManifest_)
            windowsArm64Manifests <- parse (windowsArm64Manifests__ jreManifest_)
            windowsX64Manifests   <- parse (windowsX64Manifests__ jreManifest_)
            windowsX86Manifests   <- parse (windowsX86Manifests__ jreManifest_)

            return $
                JreManifest
                    { linuxManifests_        = linuxManifests
                    , linuxI386Manifests_    = linuxI386Manifests
                    , macOSManifests_        = macOSManifests
                    , macOSArm64Manifests_   = macOSArm64Manifests
                    , windowsArm64Manifests_ = windowsArm64Manifests
                    , windowsX64Manifests_   = windowsX64Manifests
                    , windowsX86Manifests_   = windowsX86Manifests
                    }

        Left err ->
            Left err
    where
        parse :: Object -> Except String [JavaRuntimeManifest]
        parse obj = concatForM (KM.toList obj) $ \(key, val) -> do
            maybeParsed <- except (parseEither parseJSON val) <&> listToMaybe . map javaRuntimeManifest_
            case maybeParsed of
                Just parsed ->
                    return
                        [ JavaRuntimeManifest
                            { javaRuntimeManifestId_  = toString key
                            , javaRuntimeManifestSha1_ = javaRuntimeManifestSha1__ parsed
                            , javaRuntimeManifestUrl_  = javaRuntimeManifestUrl__ parsed
                            }
                        ]
                Nothing ->
                    return []

getJavaRuntimeManifestId :: JavaRuntimeManifest -> String
getJavaRuntimeManifestId = javaRuntimeManifestId_

getJavaRuntimeManifestSha1 :: JavaRuntimeManifest -> String
getJavaRuntimeManifestSha1 = javaRuntimeManifestSha1_

getJavaRuntimeManifestUrl :: JavaRuntimeManifest -> String
getJavaRuntimeManifestUrl = javaRuntimeManifestUrl_

getJavaRuntimeManifest :: JreManifest -> String -> Maybe JavaRuntimeManifest
getJavaRuntimeManifest jreManifest manifestId =
    find ((== manifestId) . getJavaRuntimeManifestId) $
        case currentOSType of
            Windows ->
                case currentOSArch of
                    Arm64  -> windowsArm64Manifests_ jreManifest
                    X86_64 -> windowsX64Manifests_ jreManifest
                    X86    -> windowsX86Manifests_ jreManifest
            OSX ->
                case currentOSArch of
                    Arm64 -> macOSArm64Manifests_ jreManifest
                    _     -> macOSManifests_ jreManifest
            Linux ->
                case currentOSArch of
                    X86 -> linuxI386Manifests_ jreManifest
                    _   -> linuxManifests_ jreManifest

