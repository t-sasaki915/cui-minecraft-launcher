module Game.Minecraft.MinecraftFiles
    ( MinecraftGameDir
    , AssetVersion
    , getDefaultMinecraftGameDir
    , getMinecraftAssetsDir
    , getMinecraftAssetIndexDir
    , getMinecraftAssetIndexPath
    , getMinecraftBinDir
    , getMinecraftLibrariesDir
    , getMinecraftVersionsDir
    , getVersionManifestPath
    , createMinecraftDirectoriesIfMissing
    , getMinecraftVersionDir
    , getClientJsonPath
    , getClientJarPath
    ) where

import           Data.Function                  ((&))
import           Data.Functor                   ((<&>))
import           Data.Minecraft.VersionManifest (MCVersionID)
import           System.Directory               (createDirectoryIfMissing,
                                                 getHomeDirectory)
import           System.FilePath                ((</>))
import           System.OperatingSystem         (OSType (..), currentOSType)
import           Text.Printf                    (printf)

type MinecraftGameDir = FilePath
type AssetVersion = String

getDefaultMinecraftGameDir :: IO FilePath
getDefaultMinecraftGameDir = case currentOSType of
    Windows ->
        getHomeDirectory <&> (</> "AppData" </> "Roaming" </> ".minecraft")
    Linux ->
        getHomeDirectory <&> (</> ".minecraft")
    OSX ->
        getHomeDirectory <&> (</> "Library" </> "Application Support" </> "minecraft")

getMinecraftAssetsDir :: MinecraftGameDir -> FilePath
getMinecraftAssetsDir = (</> "assets")

getMinecraftAssetIndexDir :: MinecraftGameDir -> FilePath
getMinecraftAssetIndexDir = (</> "indexes") . getMinecraftAssetsDir

getMinecraftAssetIndexPath :: AssetVersion -> MinecraftGameDir -> FilePath
getMinecraftAssetIndexPath assetIndex = (</> printf "%s.json" assetIndex) . getMinecraftAssetIndexDir

getMinecraftBinDir :: MinecraftGameDir -> FilePath
getMinecraftBinDir = (</> "bin")

getMinecraftLibrariesDir :: MinecraftGameDir -> FilePath
getMinecraftLibrariesDir = (</> "libraries")

getMinecraftVersionsDir :: MinecraftGameDir -> FilePath
getMinecraftVersionsDir = (</> "versions")

getVersionManifestPath :: MinecraftGameDir -> FilePath
getVersionManifestPath = (</> "version_manifest.json") . getMinecraftVersionsDir

createMinecraftDirectoriesIfMissing :: MinecraftGameDir -> IO ()
createMinecraftDirectoriesIfMissing mcDir =
    mapM_ (createDirectoryIfMissing True . (mcDir &))
        [ id
        , getMinecraftAssetsDir
        , getMinecraftAssetIndexDir
        , getMinecraftBinDir
        , getMinecraftLibrariesDir
        , getMinecraftVersionsDir
        ]


getMinecraftVersionDir :: MCVersionID -> MinecraftGameDir -> FilePath
getMinecraftVersionDir vID mcDir = getMinecraftVersionsDir mcDir </> vID

getClientJsonPath :: MCVersionID -> MinecraftGameDir -> FilePath
getClientJsonPath vID mcDir = getMinecraftVersionDir vID mcDir </> printf "%s.json" vID

getClientJarPath :: MCVersionID -> MinecraftGameDir -> FilePath
getClientJarPath vID mcDir = getMinecraftVersionDir vID mcDir </> printf "%s.jar" vID
