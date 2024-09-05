module Procedure.MinecraftLauncher (launchMinecraft) where

import           Internal.AppState
import           Procedure.MinecraftLauncher.LaunchPrepare (prepareMinecraftLaunch)

import           Control.Monad.Trans.Class                 (lift)
import qualified Data.ByteString                           as BS
import           Data.Minecraft.ClientJson
import           Data.Minecraft.VersionManifestV2          (MCVersion)
import           GHC.Stack                                 (HasCallStack)

readClientJson :: HasCallStack => MCVersion -> AppStateT IO ClientJson
readClientJson mcVersion = do
    minecraftDir <- getMinecraftDir

    let localClientJsonPath = getLocalClientJsonPath minecraftDir mcVersion

    rawJson <- lift (BS.readFile localClientJsonPath)

    return (either error id (parseClientJson rawJson))

constructGameArguments :: RuleContext -> ClientJson -> AppStateT IO [String]
constructGameArguments ruleContext clientJson =
    let gameArguments = getClientGameArguments clientJson in
        return (filterArguments ruleContext gameArguments)

constructJvmArguments :: RuleContext -> ClientJson -> AppStateT IO [String]
constructJvmArguments ruleContext clientJson =
    let jvmArguments = getClientJvmArguments clientJson in
        return (filterArguments ruleContext jvmArguments)

launchMinecraft :: HasCallStack => MCVersion -> AppStateT IO ()
launchMinecraft mcVersion = do
    prepareMinecraftLaunch mcVersion

    clientJson <- readClientJson mcVersion

    osVer <- getOSVersion
    let ruleContext =
            RuleContext
                { osVersion = osVer
                , isDemoUser = False
                , hasCustomResolution = False
                , hasQuickPlaysSupport = False
                , isQuickPlaySinglePlayer = False
                , isQuickPlayMultiplayer = False
                , isQuickPlayRealms = False
                }

    gameArguments <- constructGameArguments ruleContext clientJson
    jvmArguments  <- constructJvmArguments ruleContext clientJson

    lift (print jvmArguments)
    lift (print gameArguments)

