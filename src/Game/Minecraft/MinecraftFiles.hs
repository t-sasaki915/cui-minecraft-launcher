module Game.Minecraft.MinecraftFiles
    ( MinecraftGameDir
    , getDefaultMinecraftGameDir
    , getMinecraftAssetsDir
    , getMinecraftBinDir
    , getMinecraftLibrariesDir
    , getMinecraftVersionsDir
    , getVersionManifestPath
    , createMinecraftDirectoriesIfMissing
    ) where

import           Data.Function          ((&))
import           Data.Functor           ((<&>))
import           System.Directory       (createDirectoryIfMissing,
                                         getHomeDirectory)
import           System.FilePath        ((</>))
import           System.OperatingSystem (OSType (..), currentOSType)

type MinecraftGameDir = FilePath

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
        , getMinecraftBinDir
        , getMinecraftLibrariesDir
        , getMinecraftVersionsDir
        ]
