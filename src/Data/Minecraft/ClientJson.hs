module Data.Minecraft.ClientJson
    ( getLocalClientJsonPath
    ) where

import           Data.Minecraft                 (MinecraftDir)
import           Data.Minecraft.VersionManifest (MCVersion, getMCVersionID)
import           System.FilePath                ((</>))
import           Text.Printf                    (printf)

getLocalClientJsonPath :: MinecraftDir -> MCVersion -> FilePath
getLocalClientJsonPath mcDir mcVersion =
    let versionID = getMCVersionID mcVersion in
        mcDir </> "versions" </> versionID </> printf "%s.json" versionID
