module Data.Minecraft.ClientJson
    ( AssetVersion
    , JavaClass
    , ClientJson (..)
    , parseClientJson
    ) where

import           Data.Aeson
import           Data.ByteString          (pack)
import           Data.ByteString.Internal (c2w)
import           Text.Printf              (printf)

type AssetVersion = String
type JavaClass    = String

data ClientJson = ClientJson
    { clientArguments       :: Maybe Object -- ClientArguments
    , clientArgumentsLegacy :: Maybe String
    , clientAssetIndex      :: Object      -- AssetIndex
    , clientAssets          :: AssetVersion
    , clientDownloads       :: Object       -- Downloads
    , clientJavaVersion     :: Maybe Object     -- JavaVersion
    , clientLibraries       :: [Object] -- ClientLibrary
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
            <*> (m .:  "downloads")
            <*> (m .:? "javaVersion")
            <*> (m .:  "libraries")
            <*> (m .:  "mainClass")
    parseJSON x = fail (printf "Invalid client.json structure: %s" (show x))

parseClientJson :: String -> Either String ClientJson
parseClientJson rawJson =
    case eitherDecodeStrict' (pack $ map c2w rawJson) of
        Right versionManifest ->
            Right versionManifest

        Left err ->
            Left (printf "Failed to parse client.json: %s" err)
