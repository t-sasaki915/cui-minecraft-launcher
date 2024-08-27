module Internal.CommandLineOption
    ( CommandLineOption
    , parseCommandLineOption
    , getMinecraftDir_
    ) where

import           Data.Minecraft               (MinecraftDir,
                                               getDefaultMinecraftDir)
import           Data.Version                 (showVersion)
import           Options.Applicative
import           System.OS                    (currentOSType)
import           Text.Printf                  (printf)

import           Paths_cui_minecraft_launcher (version)

newtype CommandLineOption = CommandLineOption
    {  minecraftDir_ :: MinecraftDir
    }

getCommandLineOptionParser :: IO (Parser CommandLineOption)
getCommandLineOptionParser = do
    defaultMinecraftDir <- getDefaultMinecraftDir
    return $
        CommandLineOption
            <$> strOption
                ( help "Use a custom Minecraft game directory."
               <> value defaultMinecraftDir
               <> showDefault
               <> metavar "FilePath"
               <> long "minecraft-dir"
               <> short 'm'
                )

parseCommandLineOption :: IO CommandLineOption
parseCommandLineOption = do
    parser <- getCommandLineOptionParser
    customExecParser (prefs disambiguate)
        (info (helper <*> parser)
            (fullDesc <> header
                (printf "This is cui-minecraft-launcher %s (%s) by TSasaki."
                    (showVersion version) (show currentOSType))))

getMinecraftDir_ :: CommandLineOption -> MinecraftDir
getMinecraftDir_ = minecraftDir_
