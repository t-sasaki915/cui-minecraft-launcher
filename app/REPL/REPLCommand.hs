module REPL.REPLCommand (REPLCommand(..)) where

import           Imports

import           AppState            (AppStateT, putStrLn')

import           Options.Applicative hiding (command)

class REPLCommand a where
    commandDesc      :: a -> String
    commandArgParser :: a -> Maybe (AppStateT IO (Parser a))
    commandProcedure :: HasCallStack => a -> AppStateT IO ()

    executeREPLCommand :: HasCallStack => a -> String -> [String] -> AppStateT IO ()
    executeREPLCommand command commandLabel commandArgs = do
        argParser <-
            case commandArgParser command of
                Just parser -> parser
                Nothing     -> return (pure command)

        let argParseResult = commandArgs |> execParserPure
                (prefs disambiguate)
                    (info (helper <*> argParser)
                        (fullDesc <> progDesc (commandDesc command)))

        case argParseResult of
            Success parsedArgs ->
                commandProcedure parsedArgs

            Failure err ->
                let (helpMessage, _, _) = execFailure err commandLabel in
                    putStrLn' (show helpMessage)

            CompletionInvoked _ ->
                return ()

    internalExecuteREPLCommand :: HasCallStack => a -> [String] -> AppStateT IO ()
    internalExecuteREPLCommand command commandArgs = do
        argParser <-
            case commandArgParser command of
                Just parser -> parser
                Nothing     -> return (pure command)

        let argParseResult = commandArgs |> execParserPure
                (prefs disambiguate)
                    (info (helper <*> argParser)
                        (fullDesc <> progDesc (commandDesc command)))

        case argParseResult of
            Success parsedArgs ->
                commandProcedure parsedArgs

            Failure err ->
                let (helpMessage, _, _) = execFailure err "INTERNAL" in
                    error (printf "Failed to execute an internal command: %s." (show helpMessage))

            CompletionInvoked _ ->
                return ()
