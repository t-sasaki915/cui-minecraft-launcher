module Main (main) where

import           System.IO         (hFlush, stdout)
import           System.OS         (currentOSType)
import           System.OS.Version (fetchOSVersion)
import           Text.Printf       (printf)

main :: IO ()
main = do
    putStr "Checking OS version ..."
    hFlush stdout

    osVersion <- fetchOSVersion

    putStrLn (printf "%s %s" (show currentOSType) osVersion)
