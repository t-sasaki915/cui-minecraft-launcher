module Procedure.MinecraftLauncher (LaunchContext (..), launchMinecraft) where

import           Internal.AppState
import           Procedure.MinecraftLauncher.LaunchPrepare (prepareMinecraftLaunch)

import           Control.Monad.Trans.Class                 (lift)
import qualified Data.ByteString                           as BS
import           Data.List                                 (intercalate)
import           Data.List.Extra                           (replace)
import           Data.Minecraft.ClientJson
import           Data.Minecraft.VersionManifestV2          (MCVersion)
import           Data.Version                              (showVersion)
import           GHC.Stack                                 (HasCallStack)
import           System.OS                                 (OSType (..),
                                                            currentOSType)
import           Text.Printf                               (printf)

import           Data.Maybe                                (isJust)
import           Paths_cui_minecraft_launcher              (version)

data LaunchContext = LaunchContext
    { windowWidth           :: Maybe Int
    , windowHeight          :: Maybe Int
    , shouldUseDemoMode     :: Bool
    , quickPlaySinglePlayer :: Maybe String
    , quickPlayMultiPlayer  :: Maybe String
    , quickPlayRealms       :: Maybe String
    }

getRuleContext :: LaunchContext -> AppStateT IO RuleContext
getRuleContext ctx = do
    osVer <- getOSVersion
    return $
        RuleContext
            { osVersion = osVer
            , isDemoUser = shouldUseDemoMode ctx
            , hasCustomResolution = isJust (windowWidth ctx)
            , hasQuickPlaysSupport = True
            , isQuickPlaySinglePlayer = isJust (quickPlaySinglePlayer ctx)
            , isQuickPlayMultiplayer = isJust (quickPlayMultiPlayer ctx)
            , isQuickPlayRealms = isJust (quickPlayRealms ctx)
            }

readClientJson :: HasCallStack => MCVersion -> AppStateT IO ClientJson
readClientJson mcVersion = do
    minecraftDir <- getMinecraftDir

    let localClientJsonPath = getLocalClientJsonPath minecraftDir mcVersion

    rawJson <- lift (BS.readFile localClientJsonPath)

    return (either error id (parseClientJson rawJson))

constructArguments :: [String] -> [(String, String)] -> [String]
constructArguments  = foldl $ \args (bindKey, bindValue) ->
    flip map args $ replace (printf "${%s}" bindKey) bindValue

constructGameArguments :: RuleContext -> ClientJson -> AppStateT IO [String]
constructGameArguments ruleContext clientJson =
    let gameArguments = getClientGameArguments clientJson in
        return (filterArguments ruleContext gameArguments)

constructClasspath :: RuleContext -> ClientJson -> AppStateT IO String
constructClasspath ruleContext clientJson = do
    minecraftDir <- getMinecraftDir

    let clientLibraries = getClientLibraries clientJson
        adoptedLibraries = filterLibraries ruleContext clientLibraries
        libraryPaths = map (getLocalLibraryPath minecraftDir) adoptedLibraries
        localClientJarPath = getLocalClientJarPath minecraftDir clientJson

    return (intercalate separator (libraryPaths ++ [localClientJarPath]))
    where
        separator = case currentOSType of
            Windows -> ";"
            OSX     -> ":"
            Linux   -> ":"

constructJvmArguments :: RuleContext -> ClientJson -> AppStateT IO [String]
constructJvmArguments ruleContext clientJson = do
    minecraftDir <- getMinecraftDir

    let jvmArguments = getClientJvmArguments clientJson
        launcherVersion = showVersion version
        localClientJarPath = getLocalClientJarPath minecraftDir clientJson

    classpath <- constructClasspath ruleContext clientJson

    return $ constructArguments (filterArguments ruleContext jvmArguments)
        [ ("natives_directory", "TODO")
        , ("launcher_name"    , "cui-minecraft-launcher")
        , ("launcher_version" , launcherVersion)
        , ("client_jar_path"  , localClientJarPath)
        , ("classpath"        , classpath)
        ]

launchMinecraft :: HasCallStack => MCVersion -> LaunchContext -> AppStateT IO ()
launchMinecraft mcVersion launchCtx = do
    ruleContext <- getRuleContext launchCtx

    prepareMinecraftLaunch mcVersion ruleContext

    clientJson <- readClientJson mcVersion

    gameArguments <- constructGameArguments ruleContext clientJson
    jvmArguments  <- constructJvmArguments ruleContext clientJson

    lift (print jvmArguments)
    lift (print gameArguments)

