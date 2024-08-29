module Data.Minecraft.AssetIndex
    ( getLocalAssetIndexPath
    ) where

import           Data.Minecraft            (MinecraftDir)
import           Data.Minecraft.ClientJson (ClientJson, getAssetVersion)
import           System.FilePath           ((</>))
import           Text.Printf               (printf)

getLocalAssetIndexPath :: MinecraftDir -> ClientJson -> FilePath
getLocalAssetIndexPath mcDir clientJson =
    let assetVersion = getAssetVersion clientJson in
        mcDir </> "assets" </> "indexes" </> printf "%s.json" assetVersion
