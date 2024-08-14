module Data.Minecraft.ClientJson
    ( AssetVersion
    , JavaClass
    , ClientArguments (..)
    , RuleAction (..)
    , Rule (..)
    , OSRule (..)
    , FeatureRule (..)
    , ClientArgument (..)
    , AssetIndex (..)
    , ClientDownload (..)
    , ClientDownloads (..)
    , JavaVersion (..)
    , ClientLibrary (..)
    , LibraryDownloads (..)
    , LibraryArtifact (..)
    , LibraryClassifiers (..)
    , ClientJson (..)
    , RuleContext (..)
    , parseClientJson
    , processRules
    ) where

import           Control.Monad                    (forM)
import           Control.Monad.Trans.State.Strict (execState, put)
import           Data.Aeson
import           Data.ByteString                  (ByteString)
import           Data.Functor                     ((<&>))
import           Data.Text                        (unpack)
import           System.OperatingSystem           (OSArch, OSType,
                                                   currentOSArch, currentOSType)
import           Text.Printf                      (printf)
import           Text.Regex.Posix                 ((=~))

type AssetVersion = String
type JavaClass    = String

data ClientArguments = ClientArguments
    { clientGameArguments :: [ClientArgument]
    , clientJvmArguments  :: [ClientArgument]
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
    { ruleAction  :: RuleAction
    , osRule      :: Maybe OSRule
    , featureRule :: Maybe FeatureRule
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
    { osNameRule       :: Maybe OSType
    , osVersionPattern :: Maybe String
    , osArchNameRule   :: Maybe OSArch
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
    { isDemoUserRule          :: Maybe Bool
    , hasCustomResolutionRule :: Maybe Bool
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
    { assetId  :: AssetVersion
    , assetUrl :: String
    }
    deriving Show

instance FromJSON AssetIndex where
    parseJSON (Object m) =
        AssetIndex
            <$> (m .: "id")
            <*> (m .: "url")
    parseJSON x = fail (printf "Invalid AssetIndex structure: %s" (show x))

newtype ClientDownload = ClientDownload
    { clientDownloadUrl :: String
    }
    deriving Show

instance FromJSON ClientDownload where
    parseJSON (Object m) =
        ClientDownload
            <$> (m .: "url")
    parseJSON x = fail (printf "Invalid ClientDownload structure: %s" (show x))

newtype ClientDownloads = ClientDownloads
    { clientDownload :: ClientDownload
    }
    deriving Show

instance FromJSON ClientDownloads where
    parseJSON (Object m) =
        ClientDownloads
            <$> (m .: "client")
    parseJSON x = fail (printf "Invalid ClientDownloads structure: %s" (show x))

data JavaVersion = JavaVersion
    { javaVersionComponent :: String
    , javaMajorVersion     :: Int
    }
    deriving Show

instance FromJSON JavaVersion where
    parseJSON (Object m) =
        JavaVersion
            <$> (m .: "component")
            <*> (m .: "majorVersion")
    parseJSON x = fail (printf "Invalid JavaVersion structure: %s" (show x))

data ClientLibrary = ClientLibrary
    { libraryDownloads :: LibraryDownloads
    , libraryRules     :: Maybe [Rule]
    }
    deriving Show

instance FromJSON ClientLibrary where
    parseJSON (Object m) =
        ClientLibrary
            <$> (m .:  "downloads")
            <*> (m .:? "rules")
    parseJSON x = fail (printf "Invalid ClientLibrary structure: %s" (show x))

data LibraryDownloads = LibraryDownloads
    { libraryArtifact    :: Maybe LibraryArtifact
    , libraryClassifiers :: Maybe LibraryClassifiers
    }
    deriving Show

instance FromJSON LibraryDownloads where
    parseJSON (Object m) =
        LibraryDownloads
            <$> (m .:? "artifact")
            <*> (m .:? "classifiers")
    parseJSON x = fail (printf "Invalid LibraryDownloads structure: %s" (show x))

data LibraryArtifact = LibraryArtifact
    { libraryArtifactPath :: FilePath
    , libraryArtifactUrl  :: String
    }
    deriving Show

instance FromJSON LibraryArtifact where
    parseJSON (Object m) =
        LibraryArtifact
            <$> (m .: "path")
            <*> (m .: "url")
    parseJSON x = fail (printf "Invalid LibraryArtifact structure: %s" (show x))

data LibraryClassifiers = LibraryClassifiers
    { libraryNativesLinux   :: Maybe LibraryArtifact
    , libraryNativesOSX     :: Maybe LibraryArtifact
    , libraryNativesWindows :: Maybe LibraryArtifact
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
    { clientArguments       :: Maybe ClientArguments
    , clientArgumentsLegacy :: Maybe String
    , clientAssetIndex      :: AssetIndex
    , clientAssets          :: AssetVersion
    , clientVersionId       :: String
    , clientDownloads       :: ClientDownloads
    , clientJavaVersion     :: Maybe JavaVersion
    , clientLibraries       :: [ClientLibrary]
    , clientMainClass       :: JavaClass
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

parseClientJson :: ByteString -> Either String ClientJson
parseClientJson rawJson =
    case eitherDecodeStrict' rawJson of
        Right clientJson ->
            Right clientJson

        Left err ->
            Left (printf "Failed to parse client.json: %s" err)

data RuleContext = RuleContext
    { osVersion           :: String
    , isDemoUser          :: Bool
    , hasCustomResolution :: Bool
    }

processRule :: RuleContext -> Rule -> Bool
processRule _ (Rule Allow Nothing Nothing) =
    True
processRule ctx (Rule Allow (Just osrule) Nothing) =
    processOSRule ctx osrule
processRule ctx (Rule Allow Nothing (Just featurerule)) =
    processFeatureRule ctx featurerule
processRule ctx (Rule Allow (Just osrule) (Just featurerule)) =
    processOSRule ctx osrule && processFeatureRule ctx featurerule
processRule _ (Rule Disallow Nothing Nothing) =
    False
processRule ctx (Rule Disallow (Just osrule) Nothing) =
    not (processOSRule ctx osrule)
processRule ctx (Rule Disallow Nothing (Just featurerule)) =
    not (processFeatureRule ctx featurerule)
processRule ctx (Rule Disallow (Just osrule) (Just featurerule)) =
    not (processOSRule ctx osrule && processFeatureRule ctx featurerule)

processFeatureRule :: RuleContext -> FeatureRule -> Bool
processFeatureRule _ (FeatureRule Nothing Nothing) =
    True
processFeatureRule (RuleContext _ demo _) (FeatureRule (Just demo_) Nothing) =
    demo == demo_
processFeatureRule (RuleContext _ _ res) (FeatureRule Nothing (Just res_)) =
    res == res_
processFeatureRule (RuleContext _ demo res) (FeatureRule (Just demo_) (Just res_)) =
    demo == demo_ && res == res_

processOSRule :: RuleContext -> OSRule -> Bool
processOSRule _ (OSRule Nothing Nothing Nothing) =
    True
processOSRule _ (OSRule (Just osType) Nothing Nothing) =
    osType == currentOSType
processOSRule (RuleContext osVer _ _) (OSRule Nothing (Just verPattern) Nothing) =
    osVer =~ (verPattern :: String)
processOSRule _ (OSRule Nothing Nothing (Just osArch)) =
    osArch == currentOSArch
processOSRule (RuleContext osVer _ _) (OSRule (Just osType) (Just verPattern) Nothing) =
    osType == currentOSType && osVer =~ (verPattern :: String)
processOSRule (RuleContext osVer _ _) (OSRule Nothing (Just verPattern) (Just osArch)) =
    osVer =~ (verPattern :: String) && osArch == currentOSArch
processOSRule _ (OSRule (Just osType) Nothing (Just osArch)) =
    osType == currentOSType && osArch == currentOSArch
processOSRule (RuleContext osVer _ _) (OSRule (Just osType) (Just verPattern) (Just osArch)) =
    osType == currentOSType && osVer =~ (verPattern :: String) && osArch == currentOSArch

processRules :: RuleContext -> [Rule] -> Bool
processRules _ [] = True
processRules ctx rules = flip execState False $
    forM rules $ \rule ->
        put (processRule ctx rule)
