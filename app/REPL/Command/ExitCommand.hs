module REPL.Command.ExitCommand (ExitCommand (ExitCommand)) where

import           Imports

import           AppState         (AppStateT, putStrLn')
import           REPL.REPLCommand (REPLCommand (..))
import           System.Exit      (exitSuccess)

data ExitCommand = ExitCommand

instance REPLCommand ExitCommand where
    commandDesc = const "Exit the program."

    commandArgParser = const Nothing

    commandProcedure = exitCommandProcedure

exitCommandProcedure :: ExitCommand -> AppStateT IO ()
exitCommandProcedure _ = do
    putStrLn' "Exiting..."

    lift exitSuccess
