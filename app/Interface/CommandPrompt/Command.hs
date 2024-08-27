module Interface.CommandPrompt.Command (Command (..)) where

import           Internal.AppState         (AppStateT)

import           Control.Monad.Trans.Class (lift)
import           Data.Maybe                (fromMaybe)
import           GHC.Stack                 (HasCallStack)
import           Options.Applicative       hiding (command)

class Command a where
    commandDesc      :: a -> String
    commandArgParser :: a -> Maybe (AppStateT IO (Parser a))
    commandProcedure :: HasCallStack => a -> AppStateT IO ()

    executeCommand :: HasCallStack => a -> String -> [String] -> AppStateT IO ()
    executeCommand command commandLabel commandArgs = do
        argParser <- fromMaybe (return (pure command)) (commandArgParser command)
        let argParseResult = execParserPure (prefs disambiguate)
                (info (helper <*> argParser)
                    (fullDesc <> progDesc (commandDesc command))) commandArgs

        case argParseResult of
            Success parsedArgs ->
                commandProcedure parsedArgs

            Failure err ->
                let (helpMsg, _, _) = execFailure err commandLabel in
                    lift (print helpMsg)

            CompletionInvoked _ ->
                return ()

    executeCommandInternal :: HasCallStack => a -> [String] -> AppStateT IO ()
    executeCommandInternal command = executeCommand command "INTERNAL"
