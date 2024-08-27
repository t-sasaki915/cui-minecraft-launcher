module Internal.AppState
    ( AppStateT
    , runAppStateT
    , execAppStateT
    , evalAppStateT
    , AppState
    , initialiseAppState
    , getMinecraftDir
    , getOSVersion
    ) where

import           Control.Monad.Trans.State.Strict (StateT, evalStateT,
                                                   execStateT, get, runStateT)
import           Data.Minecraft                   (MinecraftDir)
import           Internal.CommandLineOption       (CommandLineOption,
                                                   getMinecraftDir_)
import           System.OS.Version                (OSVersion)

type AppStateT = StateT AppState

runAppStateT :: AppStateT m a -> AppState -> m (a, AppState)
runAppStateT = runStateT

execAppStateT :: Monad m => AppStateT m a -> AppState -> m AppState
execAppStateT = execStateT

evalAppStateT :: Monad m => AppStateT m a -> AppState -> m a
evalAppStateT = evalStateT

data AppState = AppState
    { minecraftDir_ :: MinecraftDir
    , osVersion_    :: OSVersion
    }

initialiseAppState :: OSVersion -> CommandLineOption -> AppState
initialiseAppState osVersion appOption =
    AppState
        { minecraftDir_ = getMinecraftDir_ appOption
        , osVersion_    = osVersion
        }

getMinecraftDir :: Monad m => AppStateT m MinecraftDir
getMinecraftDir = minecraftDir_ <$> get

getOSVersion :: Monad m => AppStateT m OSVersion
getOSVersion = osVersion_ <$> get
