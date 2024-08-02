module REPL.Command.HelpCommand (HelpCommand (HelpCommand)) where

import           AppState         (AppStateT, putStrLn')
import           REPL.REPLCommand (REPLCommand (..))

data HelpCommand = HelpCommand

instance REPLCommand HelpCommand where
    commandDesc = const "Show the command reference of this REPL."

    commandArgParser = const Nothing

    commandProcedure = helpCommandProcedure

helpCommandProcedure :: HelpCommand -> AppStateT IO ()
helpCommandProcedure _ = do
    putStrLn' "HELP"
