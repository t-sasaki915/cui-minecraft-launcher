module Data.Minecraft.ClientJson
    ( ClientJson
    , getLocalClientJsonPath
    , parseClientJson
    ) where

import           Data.Aeson
import           Data.ByteString                (ByteString)
import           Data.Functor                   ((<&>))
import           Data.Minecraft                 (MinecraftDir)
import           Data.Minecraft.VersionManifest (MCVersion, MCVersionID,
                                                 getMCVersionID)
import           Data.Text                      (unpack)
import           System.FilePath                ((</>))
import           System.OS                      (OSType)
import           System.OS.Arch                 (OSArch)
import           Text.Printf                    (printf)

type AssetVersion = String
type JavaClass    = String

data ClientArguments = ClientArguments
    { clientGameArguments_ :: [ClientArgument]
    , clientJvmArguments_  :: [ClientArgument]
    }
    deriving Show

instance FromJSON ClientArguments where
    parseJSON (Object m) =
        ClientArguments
            <$> (m .: "game")
            <*> (m .: "jvm")
    parseJSON x = fail (printf "Invalid ClientArguments structure: %s" (show x))

data RuleAction = Allow | Disallow deriving Show

instance FromJSON RuleAction where
    parseJSON (String "allow")    = pure Allow
    parseJSON (String "disallow") = pure Disallow
    parseJSON x = fail (printf "Invalid RuleAction structure: %s" (show x))

data Rule = Rule
    { ruleAction_  :: RuleAction
    , osRule_      :: Maybe OSRule
    , featureRule_ :: Maybe FeatureRule
    }
    deriving Show

instance FromJSON Rule where
    parseJSON (Object m) =
        Rule
            <$> (m .:  "action")
            <*> (m .:? "os")
            <*> (m .:? "features")
    parseJSON x = fail (printf "Invalid Rule structure: %s" (show x))

data OSRule = OSRule
    { osNameRule_       :: Maybe OSType
    , osVersionPattern_ :: Maybe String
    , osArchNameRule_   :: Maybe OSArch
    }
    deriving Show

instance FromJSON OSRule where
    parseJSON (Object m) =
        OSRule
            <$> (m .:? "name")
            <*> (m .:? "version")
            <*> (m .:? "arch")
    parseJSON x = fail (printf "Invalid OSRule structure: %s" (show x))

data FeatureRule = FeatureRule
    { isDemoUserRule_          :: Maybe Bool
    , hasCustomResolutionRule_ :: Maybe Bool
    }
    deriving Show

instance FromJSON FeatureRule where
    parseJSON (Object m) =
        FeatureRule
            <$> (m .:? "is_demo_user")
            <*> (m .:? "has_custom_resolution")
    parseJSON x = fail (printf "Invalid FeatureRule structure: %s" (show x))

data RuleValue = SingleValue String
               | MultipleValue [String]
               deriving Show

instance FromJSON RuleValue where
    parseJSON (String str) = pure (SingleValue (unpack str))
    parseJSON (Array xs)   = parseJSON (Array xs) <&> MultipleValue
    parseJSON x = fail (printf "Invalid RuleValue structure: %s" (show x))

data ClientArgument = CommonClientArgument String
                    | ClientArgumentsWithRules [Rule] RuleValue
                    deriving Show

instance FromJSON ClientArgument where
    parseJSON (String str) = pure (CommonClientArgument (unpack str))
    parseJSON (Object m) =
        ClientArgumentsWithRules
            <$> (m .: "rules")
            <*> (m .: "value")
    parseJSON x = fail (printf "Invalid ClientArgument structure: %s" (show x))

data AssetIndex = AssetIndex
    { assetVersion_ :: AssetVersion
    , assetUrl_     :: String
    }
    deriving Show

instance FromJSON AssetIndex where
    parseJSON (Object m) =
        AssetIndex
            <$> (m .: "id")
            <*> (m .: "url")
    parseJSON x = fail (printf "Invalid AssetIndex structure: %s" (show x))

newtype ClientDownload = ClientDownload
    { clientDownloadUrl_ :: String
    }
    deriving Show

instance FromJSON ClientDownload where
    parseJSON (Object m) =
        ClientDownload
            <$> (m .: "url")
    parseJSON x = fail (printf "Invalid ClientDownload structure: %s" (show x))

newtype ClientDownloads = ClientDownloads
    { clientDownload_ :: ClientDownload
    }
    deriving Show

instance FromJSON ClientDownloads where
    parseJSON (Object m) =
        ClientDownloads
            <$> (m .: "client")
    parseJSON x = fail (printf "Invalid ClientDownloads structure: %s" (show x))

data JavaVersion = JavaVersion
    { javaVersionComponent_ :: String
    , javaMajorVersion_     :: Int
    }
    deriving Show

instance FromJSON JavaVersion where
    parseJSON (Object m) =
        JavaVersion
            <$> (m .: "component")
            <*> (m .: "majorVersion")
    parseJSON x = fail (printf "Invalid JavaVersion structure: %s" (show x))

data ClientLibrary = ClientLibrary
    { libraryDownloads_ :: LibraryDownloads
    , libraryRules_     :: Maybe [Rule]
    }
    deriving Show

instance FromJSON ClientLibrary where
    parseJSON (Object m) =
        ClientLibrary
            <$> (m .:  "downloads")
            <*> (m .:? "rules")
    parseJSON x = fail (printf "Invalid ClientLibrary structure: %s" (show x))

data LibraryDownloads = LibraryDownloads
    { libraryArtifact_    :: Maybe LibraryArtifact
    , libraryClassifiers_ :: Maybe LibraryClassifiers
    }
    deriving Show

instance FromJSON LibraryDownloads where
    parseJSON (Object m) =
        LibraryDownloads
            <$> (m .:? "artifact")
            <*> (m .:? "classifiers")
    parseJSON x = fail (printf "Invalid LibraryDownloads structure: %s" (show x))

data LibraryArtifact = LibraryArtifact
    { libraryArtifactPath_ :: FilePath
    , libraryArtifactUrl_  :: String
    }
    deriving Show

instance FromJSON LibraryArtifact where
    parseJSON (Object m) =
        LibraryArtifact
            <$> (m .: "path")
            <*> (m .: "url")
    parseJSON x = fail (printf "Invalid LibraryArtifact structure: %s" (show x))

data LibraryClassifiers = LibraryClassifiers
    { libraryNativesLinux_   :: Maybe LibraryArtifact
    , libraryNativesOSX_     :: Maybe LibraryArtifact
    , libraryNativesWindows_ :: Maybe LibraryArtifact
    }
    deriving Show

instance FromJSON LibraryClassifiers where
    parseJSON (Object m) =
        LibraryClassifiers
            <$> (m .:? "natives-linux")
            <*> (m .:? "natives-osx")
            <*> (m .:? "natives-windows")
    parseJSON x = fail (printf "Invalid LibraryClassifiers structure: %s" (show x))

data ClientJson = ClientJson
    { clientArguments_       :: Maybe ClientArguments
    , clientArgumentsLegacy_ :: Maybe String
    , clientAssetIndex_      :: AssetIndex
    , clientAssetVersion_    :: AssetVersion
    , clientVersionId_       :: MCVersionID
    , clientDownloads_       :: ClientDownloads
    , clientJavaVersion_     :: Maybe JavaVersion
    , clientLibraries_       :: [ClientLibrary]
    , clientMainClass_       :: JavaClass
    }
    deriving Show

instance FromJSON ClientJson where
    parseJSON (Object m) =
        ClientJson
            <$> (m .:? "arguments")
            <*> (m .:? "minecraftArguments")
            <*> (m .:  "assetIndex")
            <*> (m .:  "assets")
            <*> (m .:  "id")
            <*> (m .:  "downloads")
            <*> (m .:? "javaVersion")
            <*> (m .:  "libraries")
            <*> (m .:  "mainClass")
    parseJSON x = fail (printf "Invalid client.json structure: %s" (show x))

getLocalClientJsonPath :: MinecraftDir -> MCVersion -> FilePath
getLocalClientJsonPath mcDir mcVersion =
    let versionID = getMCVersionID mcVersion in
        mcDir </> "versions" </> versionID </> printf "%s.json" versionID

parseClientJson :: ByteString -> Either String ClientJson
parseClientJson =
    either (Left . printf "Failed to parse client.json: %s") Right .
        eitherDecodeStrict'
