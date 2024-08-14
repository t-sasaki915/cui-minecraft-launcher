module Game.Minecraft.MinecraftFiles
    ( MinecraftGameDir
    , AssetVersion
    , AssetHash
    , getDefaultMinecraftGameDir
    , getMinecraftAssetsDir
    , getMinecraftAssetIndexDir
    , getMinecraftAssetIndexPath
    , getMinecraftAssetObjectsDir
    , getMinecraftAssetObjectPrefixDir
    , getMinecraftAssetObjectPath
    , getMinecraftBinDir
    , getMinecraftLibrariesDir
    , getMinecraftVersionsDir
    , getVersionManifestPath
    , createMinecraftDirectoriesIfMissing
    , getMinecraftVersionDir
    , getMinecraftResourcesDir
    , getMinecraftResourcePath
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
type AssetHash = String

getDefaultMinecraftGameDir :: IO MinecraftGameDir
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
getMinecraftAssetIndexPath assetVersion = (</> printf "%s.json" assetVersion) . getMinecraftAssetIndexDir

getMinecraftAssetObjectsDir :: MinecraftGameDir -> FilePath
getMinecraftAssetObjectsDir = (</> "objects") . getMinecraftAssetsDir

getMinecraftAssetObjectPrefixDir :: AssetHash -> MinecraftGameDir -> FilePath
getMinecraftAssetObjectPrefixDir assetHash = (</> take 2 assetHash) . getMinecraftAssetObjectsDir

getMinecraftAssetObjectPath :: AssetHash -> MinecraftGameDir -> FilePath
getMinecraftAssetObjectPath assetHash mcDir = getMinecraftAssetObjectPrefixDir assetHash mcDir </> assetHash

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
        , getMinecraftAssetObjectsDir
        , getMinecraftBinDir
        , getMinecraftLibrariesDir
        , getMinecraftVersionsDir
        ]


getMinecraftVersionDir :: MCVersionID -> MinecraftGameDir -> FilePath
getMinecraftVersionDir vID mcDir = getMinecraftVersionsDir mcDir </> vID

getMinecraftResourcesDir :: MCVersionID -> MinecraftGameDir -> FilePath
getMinecraftResourcesDir vID mcDir = getMinecraftVersionDir vID mcDir </> "resources"

getMinecraftResourcePath :: String -> MCVersionID -> MinecraftGameDir -> FilePath
getMinecraftResourcePath rPath vID mcDir = getMinecraftResourcesDir vID mcDir </> rPath

getClientJsonPath :: MCVersionID -> MinecraftGameDir -> FilePath
getClientJsonPath vID mcDir = getMinecraftVersionDir vID mcDir </> printf "%s.json" vID

getClientJarPath :: MCVersionID -> MinecraftGameDir -> FilePath
getClientJarPath vID mcDir = getMinecraftVersionDir vID mcDir </> printf "%s.jar" vID
