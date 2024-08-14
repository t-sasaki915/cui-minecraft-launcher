module Data.Minecraft.AssetIndex
    ( AssetIndex (..)
    , AssetObject (..)
    , parseAssetIndex
    , getAssetObjects
    ) where

import           Data.Aeson
import           Data.Aeson.Key                (toString)
import qualified Data.Aeson.KeyMap             as KeyMap
import           Data.ByteString               (ByteString)
import           Data.Text                     (unpack)
import           Game.Minecraft.MinecraftFiles (AssetHash)
import           GHC.Stack                     (HasCallStack)
import           Text.Printf                   (printf)

data AssetIndex = AssetIndex
    { mapToResources :: Maybe Bool
    , virtualAsset   :: Maybe Bool
    , assetObjects   :: Object
    }
    deriving Show

instance FromJSON AssetIndex where
    parseJSON (Object m) =
        AssetIndex
            <$> (m .:? "map_to_resources")
            <*> (m .:? "virtual")
            <*> (m .:  "objects")
    parseJSON x = fail (printf "Invalid AssetIndex structure: %s" (show x))

parseAssetIndex :: ByteString -> Either String AssetIndex
parseAssetIndex rawJson =
    case eitherDecodeStrict' rawJson of
        Right assetIndex ->
            Right assetIndex

        Left err ->
            Left (printf "Failed to parse the asset index: %s" err)

data AssetObject = AssetObject
    { assetFileName :: String
    , assetHash     :: AssetHash
    }

getAssetObjects :: HasCallStack => AssetIndex -> [AssetObject]
getAssetObjects assetIndex =
    flip map (KeyMap.toList (assetObjects assetIndex)) $ \case
        (key, Object obj) ->
            case KeyMap.lookup "hash" obj of
                Just (String hash) ->
                    AssetObject
                        { assetFileName = toString key
                        , assetHash     = unpack hash
                        }

                _ ->
                    error (printf "Invalid AssetObject structure.")

        _ ->
            error (printf "Invalid AssetObject structure.")
