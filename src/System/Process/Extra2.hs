module System.Process.Extra2
    ( readProcessEither
    , execProcessEither
    ) where

import           Data.Functor   ((<&>))
import           System.Exit    (ExitCode (ExitSuccess))
import           System.Process (proc, readCreateProcessWithExitCode)

readProcessEither :: FilePath -> [String] -> IO (Either String String)
readProcessEither execName execArgs =
    readCreateProcessWithExitCode (proc execName execArgs) [] <&> \case
        (ExitSuccess, stdout, _) -> Right stdout
        (_, _, stderr)           -> Left stderr

execProcessEither :: FilePath -> [String] -> IO (Either String ())
execProcessEither execName execArgs =
    readProcessEither execName execArgs <&> either Left (const (Right ()))
