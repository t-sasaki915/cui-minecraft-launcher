module Data.Minecraft (MinecraftDir, getDefaultMinecraftDir) where

import           Data.Functor     ((<&>))
import           System.Directory (getHomeDirectory)
import           System.FilePath  ((</>))
import           System.OS        (OSType (..), currentOSType)

type MinecraftDir = FilePath

getDefaultMinecraftDir :: IO MinecraftDir
getDefaultMinecraftDir = case currentOSType of
    Windows -> getHomeDirectory <&> (</> "AppData" </> "Roaming" </> ".minecraft")
    OSX     -> getHomeDirectory <&> (</> "Library" </> "Application Support" </> "minecraft")
    Linux   -> getHomeDirectory <&> (</> ".minecraft")

