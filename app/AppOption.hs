module AppOption (AppOption(..), getAppOptionParser) where

import           CrossPlatform       (getDefaultMinecraftGameDir)
import           Options.Applicative

newtype AppOption = AppOption
    { minecraftGameDir :: FilePath
    }
    deriving Show

getAppOptionParser :: IO (Parser AppOption)
getAppOptionParser = do
    defaultMinecraftDir <- getDefaultMinecraftGameDir

    return $
        AppOption
            <$> strOption
                ( long "minecraft-dir"
               <> short 'm'
               <> metavar "FilePath"
               <> showDefault
               <> value defaultMinecraftDir
               <> help "Specify the Minecraft game directory expressly."
                )
