module Game.CuiMinecraftLauncher.Internal.Imports
    ( filterM
    , foldM
    , foldM_
    , forM
    , forM_
    , unless
    , void
    , when
    , unlessM
    , whenJust
    , whenJustM
    , whenM
    , lift
    , (&)
    , (<&>)
    , isJust
    , HasCallStack
    , doesDirectoryExist
    , doesFileExist
    , (</>)
    , printf
    , (=~)
    , version
    ) where

import           Control.Monad                (filterM, foldM, foldM_, forM,
                                               forM_, unless, void, when)
import           Control.Monad.Extra          (unlessM, whenJust, whenJustM,
                                               whenM)
import           Control.Monad.Trans.Class    (lift)
import           Data.Function                ((&))
import           Data.Functor                 ((<&>))
import           Data.Maybe                   (isJust)
import           GHC.Stack                    (HasCallStack)
import           System.Directory             (doesDirectoryExist,
                                               doesFileExist)
import           System.FilePath              ((</>))
import           Text.Printf                  (printf)
import           Text.Regex.Posix             ((=~))

import           Paths_cui_minecraft_launcher (version)
