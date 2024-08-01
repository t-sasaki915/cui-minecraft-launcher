module Main (main) where

import           Imports

import           AppOption           (getAppOptionParser)
import           CrossPlatform       (currentOSType)

import           Data.Version        (showVersion)
import           Options.Applicative

main :: IO ()
main = do
    appOptionParser <- getAppOptionParser
    appOption <- customExecParser (prefs disambiguate)
        (info (helper <*> appOptionParser)
            (fullDesc <> header
                (printf "This is cui-minecraft-launcher %s (%s) by TSasaki."
                    (showVersion version) (show currentOSType))))

    print appOption
