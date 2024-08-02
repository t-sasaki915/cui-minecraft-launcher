{-# LANGUAGE ScopedTypeVariables #-}

module REPL.REPLMain (replMain) where

import           Imports

import           AppState                 (AppStateT, getAppState, putAppState,
                                           runAppStateT)
import           CrossPlatform            (currentOSType)
import           REPL.Command.HelpCommand (HelpCommand (HelpCommand))
import           REPL.REPLCommand         (REPLCommand (..))

import           Control.Exception        (SomeException (..), throw, try)
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
                let
                    execute :: HasCallStack => REPLCommand c => c -> AppStateT IO ()
                    execute command = executeREPLCommand command commandLabel commandArgs

                    execution =
                        case commandLabel of
                            "help" -> execute HelpCommand
                            _      -> error (printf "Command '%s' is unknown." commandLabel)

                currentAppState <- lift getAppState
                executionResult <- lift (lift (try (runAppStateT execution currentAppState)))

                case executionResult of
                    Right ((), newState) -> do
                        lift (putAppState newState)
                        repLoop

                    Left (err :: SomeException) -> do
                        when (show err =~ ("^Exit(Success|Failure)( [0-9]+)?$" :: String)) $
                            throw err

                        outputStrLn (show err)
                        repLoop

            [] ->
                return ()

        repLoop
