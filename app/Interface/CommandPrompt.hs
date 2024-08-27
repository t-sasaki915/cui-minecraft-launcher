module Interface.CommandPrompt (startCommandPrompt) where

import           Internal.AppState            (AppStateT)

import           Control.Monad.Extra          (whenJustM)
import           Control.Monad.Trans.Class    (lift)
import           Data.Version                 (showVersion)
import           System.Console.Haskeline
import           System.OS                    (currentOSType)
import           Text.Printf                  (printf)

import           Paths_cui_minecraft_launcher (version)

commandPrompt :: InputT (AppStateT IO) ()
commandPrompt =
    whenJustM (getInputLine "cui-minecraft-launcher> ") $ \input -> do
        outputStrLn input

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
