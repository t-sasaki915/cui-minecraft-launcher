module REPL.Command.HelpCommand (HelpCommand (HelpCommand), commandAndDescriptions) where

import           Imports

import           AppState         (AppStateT, putStrLn')
import           Data.Version     (showVersion)
import           REPL.REPLCommand (REPLCommand (..))

data HelpCommand = HelpCommand

instance REPLCommand HelpCommand where
    commandDesc = const "Show the command reference of this REPL."

    commandArgParser = const Nothing

    commandProcedure = helpCommandProcedure

helpCommandProcedure :: HelpCommand -> AppStateT IO ()
helpCommandProcedure _ = do
    putStrLn' (printf "cui-minecraft-launcher %s Command Reference" (showVersion version))
    putStrLn' "For more informations about each command, please refer '<Command> --help'."
    putStrLn' ""

    let maxCommandLabelLength = maximum $ map (length . fst) commandAndDescriptions
        formatter = printf "%%-%ds : %%s" maxCommandLabelLength
    mapM_ (putStrLn' . uncurry (printf formatter)) commandAndDescriptions

commandAndDescriptions :: [(String, String)]
commandAndDescriptions =
    [ "help"         ~> "Show the command reference of this REPL."
    , "exit"         ~> "Exit the program."
    , "listVersions" ~> "Show the list of available Minecraft versions."
    , "quickLaunch"  ~> "Launch a Minecraft client without making an installation."
    ]
    where (~>) a b = (a, b)
