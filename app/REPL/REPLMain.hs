module REPL.REPLMain (replMain) where

import           Imports

import           AppState                 (AppStateT)
import           CrossPlatform            (currentOSType)

import           Data.Version             (showVersion)
import           System.Console.Haskeline (InputT, getInputLine, outputStrLn)

replMain :: InputT (AppStateT IO) ()
replMain = do
    outputStrLn $
        printf "This is cui-minecraft-launcher %s (%s) by TSasaki."
            (showVersion version) (show currentOSType)

    outputStrLn ""
    outputStrLn "For the command reference, type 'help'."
    outputStrLn "To exit the program gracefully, type 'exit'."
    outputStrLn ""

    repLoop

repLoop :: InputT (AppStateT IO) ()
repLoop = do
    whenJustM (getInputLine "REPL> ") $ \input -> do
        case words input of
            (commandLabel : commandArgs) -> do
                outputStrLn commandLabel
                outputStrLn (show commandArgs)

            [] ->
                return ()

        repLoop
