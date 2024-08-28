{-# LANGUAGE ScopedTypeVariables #-}

module Interface.CommandPrompt (startCommandPrompt) where

import           Interface.CommandPrompt.Command                    (Command (executeCommand))
import           Interface.CommandPrompt.Command.ExitCommand
import           Interface.CommandPrompt.Command.HelpCommand
import           Interface.CommandPrompt.Command.ListVersionCommand
import           Internal.AppState

import           Control.Exception                                  (SomeException (..),
                                                                     throw, try)
import           Control.Monad.Extra                                (when,
                                                                     whenJustM)
import           Control.Monad.Trans.Class                          (lift)
import           Data.Char                                          (toLower)
import           Data.Version                                       (showVersion)
import           System.Console.Haskeline
import           System.OS                                          (currentOSType)
import           Text.Printf                                        (printf)
import           Text.Regex.Posix                                   ((=~))

import           Paths_cui_minecraft_launcher                       (version)

commandPrompt :: InputT (AppStateT IO) ()
commandPrompt =
    whenJustM (getInputLine "cui-minecraft-launcher> ") $ \input -> do
        case words input of
            (commandLabel : commandArgs) -> do
                let execute command = executeCommand command commandLabel commandArgs
                    execution = case map toLower commandLabel of
                        "exit"        -> execute ExitCommand
                        "help"        -> execute HelpCommand
                        "listversion" -> execute ListVersionCommand
                        _      -> error (printf "Unknown command: %s" commandLabel)

                currentAppState <- lift getAppState
                executionResult <- lift (lift (try (runAppStateT execution currentAppState)))

                case executionResult of
                    Right ((), newState) ->
                        lift (putAppState newState)

                    Left (err :: SomeException) -> do
                        when (show err =~ ("^Exit(Success|Failure [0-9]+)$" :: String)) $
                            throw err

                        outputStrLn (show err)

            [] ->
                return ()

        commandPrompt

startCommandPrompt :: AppStateT IO ()
startCommandPrompt = do
    lift $ do
        putStrLn ""
        putStrLn $
            printf "This is cui-minecraft-launcher %s (%s) by TSasaki."
                (showVersion version) (show currentOSType)
        putStrLn "Type 'help' to show a command list."
        putStrLn "Type 'exit' to exit the program gracefully."
        putStrLn ""

    runInputT defaultSettings commandPrompt
