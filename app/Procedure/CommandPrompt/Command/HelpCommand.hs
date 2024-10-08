{-# LANGUAGE NoOverloadedStrings #-}

module Procedure.CommandPrompt.Command.HelpCommand (HelpCommand (HelpCommand)) where

import           Interface.CommandPrompt.Command (Command (..))
import           Internal.AppState               (AppStateT)

import           Control.Monad.Trans.Class       (lift)
import           Text.Layout.Table

data HelpCommand = HelpCommand

instance Command HelpCommand where
    commandDesc = const "Show a command list."

    commandArgParser = const Nothing

    commandProcedure = helpCommandProcedure

helpCommandProcedure :: HelpCommand -> AppStateT IO ()
helpCommandProcedure _ = do
    let rgs = map rowG
            [ ["exit", "Exit the program."]
            , ["help", "Show a command list."]
            , ["listVersion", "Show a list of available Minecraft versions."]
            , ["quickLaunch", "Launch a Minecraft client without making an installation."]
            ]

        table = columnHeaderTableS
            [defColSpec, defColSpec] asciiS (titlesH ["Command", "Description"]) rgs

    lift (putStrLn (tableString table))
