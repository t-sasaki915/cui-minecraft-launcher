module Game.Minecraft.MojangConstants
    ( getMinecraftAssetObjectDownloadUrl
    ) where

import           Game.Minecraft.MinecraftFiles (AssetHash)
import           Text.Printf                   (printf)

getMinecraftAssetObjectDownloadUrl :: AssetHash -> String
getMinecraftAssetObjectDownloadUrl assetHash =
    printf "https://resources.download.minecraft.net/%s/%s" (take 2 assetHash) assetHash
