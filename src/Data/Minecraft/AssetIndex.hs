module Data.Minecraft.AssetIndex
    ( AssetHash
    , AssetIndex
    , isVirtualAsset
    , shouldMapToResources
    , getAssetObjects
    , getAssetFilePath
    , getAssetHash
    , getLocalAssetIndexPath
    , getAssetObjectUrl
    , getLocalAssetObjectPath
    , parseAssetIndex
    ) where

import           Control.Monad              (foldM)
import           Control.Monad.Trans.Except (runExcept, throwE)
import           Data.Aeson
import           Data.Aeson.Key             (toString)
import qualified Data.Aeson.KeyMap          as KM
import           Data.ByteString            (ByteString)
import           Data.Minecraft             (MinecraftDir)
import           Data.Minecraft.ClientJson  (ClientJson, getAssetVersion,
                                             getClientVersionID)
import           Data.Text                  (unpack)
import           System.FilePath            ((</>))
import           Text.Printf                (printf)

type AssetHash = String

data AssetObject = AssetObject
    { assetFilePath_ :: FilePath
    , assetHash_     :: AssetHash
    }

data AssetIndex = AssetIndex
    { virtualAsset_   :: Bool
    , mapToResources_ :: Bool
    , assetObjects_   :: [AssetObject]
    }

getLocalAssetIndexPath :: MinecraftDir -> ClientJson -> FilePath
getLocalAssetIndexPath mcDir clientJson =
    let assetVersion = getAssetVersion clientJson in
        mcDir </> "assets" </> "indexes" </> printf "%s.json" assetVersion

isVirtualAsset :: AssetIndex -> Bool
isVirtualAsset = virtualAsset_

shouldMapToResources :: AssetIndex -> Bool
shouldMapToResources = mapToResources_

getAssetObjects :: AssetIndex -> [AssetObject]
getAssetObjects = assetObjects_

getAssetFilePath :: AssetObject -> FilePath
getAssetFilePath = assetFilePath_

getAssetHash :: AssetObject -> AssetHash
getAssetHash = assetHash_

getAssetObjectUrl :: AssetObject -> String
getAssetObjectUrl assetObj =
    let assetHash = getAssetHash assetObj in
        printf "https://resources.download.minecraft.net/%s/%s" (take 2 assetHash) assetHash

getLocalAssetObjectPath :: MinecraftDir -> ClientJson -> AssetIndex -> AssetObject -> FilePath
getLocalAssetObjectPath mcDir clientJson assetIndex assetObj
    | isVirtualAsset assetIndex =
        let assetVersion = getAssetVersion clientJson
            assetFilePath = getAssetFilePath assetObj in
                mcDir </> "assets" </> "virtual" </> assetVersion </> assetFilePath

    | shouldMapToResources assetIndex =
        let clientVersion = getClientVersionID clientJson
            assetFilePath = getAssetFilePath assetObj in
                mcDir </> "versions" </> clientVersion </> "resources" </> assetFilePath

    | otherwise =
        let assetHash = getAssetHash assetObj in
            mcDir </> "assets" </> "objects" </> take 2 assetHash </> assetHash

data AssetIndex_ = AssetIndex_
    { virtualAsset__   :: Bool
    , mapToResources__ :: Bool
    , assetObject_     :: Object
    }
    deriving Show

instance FromJSON AssetIndex_ where
    parseJSON (Object m) =
        AssetIndex_
            <$> (m .:? "virtual" .!= False)
            <*> (m .:? "map_to_resources" .!= False)
            <*> (m .:  "objects")
    parseJSON x = fail (printf "Invalid AssetIndex structure: %s" (show x))

parseAssetIndex :: ByteString -> Either String AssetIndex
parseAssetIndex rawJson =
    case eitherDecodeStrict' rawJson of
        Right assetIndex_ ->
            let eitherAssetObjects = runExcept $
                    foldM
                        (\lst item ->
                            case item of
                                (key, Object obj) ->
                                    case KM.lookup "hash" obj of
                                        Just (String hash) ->
                                            return $ lst ++
                                                [ AssetObject
                                                    { assetFilePath_ = toString key
                                                    , assetHash_     = unpack hash
                                                    }
                                                ]

                                        x ->
                                            throwE (printf "Invalid AssetObject structure: %s" (show x))

                                x ->
                                    throwE (printf "Invalid AssetObject structure: %s" (show x))
                        )
                        []
                        (KM.toList (assetObject_ assetIndex_))
            in
            case eitherAssetObjects of
                Right assetObjects ->
                    Right $ AssetIndex
                        { virtualAsset_   = virtualAsset__ assetIndex_
                        , mapToResources_ = mapToResources__ assetIndex_
                        , assetObjects_   = assetObjects
                        }

                Left errMsg ->
                    Left errMsg

        Left errMsg ->
            Left (printf "Failed to parse AssetIndex: %s" errMsg)
