module Internal.AppState
    ( AppStateT
    , runAppStateT
    , execAppStateT
    , evalAppStateT
    , AppState
    , initialiseAppState
    , getMinecraftDir
    , getOSVersion
    , getVersionManifest
    , getJavaRuntimeManifestAll
    , getAppState
    , putAppState
    ) where

import           Control.Monad.Trans.State.Strict     (StateT, evalStateT,
                                                       execStateT, get, put,
                                                       runStateT)
import           Data.JavaRuntime.JavaRuntimeManifest (JavaRuntimeManifestAll)
import           Data.Minecraft                       (MinecraftDir)
import           Data.Minecraft.VersionManifestV2     (VersionManifestV2)
import           Internal.CommandLineOption           (CommandLineOption,
                                                       getMinecraftDir_)
import           System.OS.Version                    (OSVersion)

type AppStateT = StateT AppState

runAppStateT :: AppStateT m a -> AppState -> m (a, AppState)
runAppStateT = runStateT

execAppStateT :: Monad m => AppStateT m a -> AppState -> m AppState
execAppStateT = execStateT

evalAppStateT :: Monad m => AppStateT m a -> AppState -> m a
evalAppStateT = evalStateT

data AppState = AppState
    { minecraftDir_           :: MinecraftDir
    , osVersion_              :: OSVersion
    , versionManifest_        :: VersionManifestV2
    , javaRuntimeManifestAll_ :: JavaRuntimeManifestAll
    }

initialiseAppState :: OSVersion -> VersionManifestV2 -> JavaRuntimeManifestAll -> CommandLineOption -> AppState
initialiseAppState osVersion versionManifest javaRuntimeManifestAll appOption =
    AppState
        { minecraftDir_           = getMinecraftDir_ appOption
        , osVersion_              = osVersion
        , versionManifest_        = versionManifest
        , javaRuntimeManifestAll_ = javaRuntimeManifestAll
        }

getMinecraftDir :: Monad m => AppStateT m MinecraftDir
getMinecraftDir = minecraftDir_ <$> get

getOSVersion :: Monad m => AppStateT m OSVersion
getOSVersion = osVersion_ <$> get

getVersionManifest :: Monad m => AppStateT m VersionManifestV2
getVersionManifest = versionManifest_ <$> get

getJavaRuntimeManifestAll :: Monad m => AppStateT m JavaRuntimeManifestAll
getJavaRuntimeManifestAll = javaRuntimeManifestAll_ <$> get

getAppState :: Monad m => AppStateT m AppState
getAppState = get

putAppState :: Monad m => AppState -> AppStateT m ()
putAppState = put
