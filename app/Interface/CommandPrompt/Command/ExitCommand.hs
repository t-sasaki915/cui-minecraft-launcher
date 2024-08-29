module Interface.CommandPrompt.Command.ExitCommand (ExitCommand (ExitCommand)) where

import           Interface.CommandPrompt.Command (Command (..))
import           Internal.AppState               (AppStateT)

import           Control.Monad.Trans.Class       (MonadTrans (lift))
import           System.Exit                     (exitSuccess)

data ExitCommand = ExitCommand

instance Command ExitCommand where
    commandDesc = const "Exit the program."

    commandArgParser = const Nothing

    commandProcedure = exitCommandProcedure

exitCommandProcedure :: ExitCommand -> AppStateT IO ()
exitCommandProcedure _ = do
    lift (putStrLn "Exiting...")

    lift exitSuccess
