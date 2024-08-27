module Main (main) where

import           Internal.AppState          (evalAppStateT, getMinecraftDir,
                                             initialiseAppState)
import           Internal.CommandLineOption (parseCommandLineOption)

import           Control.Monad.Trans.Class  (MonadTrans (lift))
import           System.IO                  (hFlush, stdout)
import           System.OS                  (currentOSType)
import           System.OS.Version          (OSVersion, fetchOSVersion)
import           Text.Printf                (printf)

checkOSVersion :: IO OSVersion
checkOSVersion = do
    putStr "Checking OS version ..."
    hFlush stdout

    osVersion <- fetchOSVersion

    putStrLn (printf "%s %s" (show currentOSType) osVersion)

    return osVersion

main :: IO ()
main = do
    appOption <- parseCommandLineOption
    osVersion <- checkOSVersion

    let appState = initialiseAppState osVersion appOption

    flip evalAppStateT appState $ do
        minecraftDir <- getMinecraftDir

        lift (putStrLn (printf "Using a directory '%s'." minecraftDir))
