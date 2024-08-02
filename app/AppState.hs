module AppState
    ( AppState(..)
    , AppStateT
    , runAppStateT
    , getAppState
    , putAppState
    , putStrLn'
    , initialAppState
    ) where

import           Imports

import           AppOption                        (AppOption)

import           Control.Monad.Trans.State.Strict (StateT, get, put, runStateT)

newtype AppState = AppState
    { _appOption :: AppOption
    }

type AppStateT = StateT AppState

runAppStateT :: AppStateT m a -> AppState -> m (a, AppState)
runAppStateT = runStateT

getAppState :: Monad m => AppStateT m AppState
getAppState = get

putAppState :: Monad m => AppState -> AppStateT m ()
putAppState = put

putStrLn' :: String -> AppStateT IO ()
putStrLn' = lift . putStrLn

initialAppState :: AppOption -> AppState
initialAppState appOpt =
    AppState
        { _appOption = appOpt
        }
