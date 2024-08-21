module System.Process.Extra
    ( readProcessEither
    , execProcessEither
    ) where

import           System.Exit    (ExitCode (ExitSuccess))
import           System.Process (proc, readCreateProcessWithExitCode)

readProcessEither :: FilePath -> [String] -> IO (Either String String)
readProcessEither execName execArgs = do
    (exitCode, stdout, stderr) <- readCreateProcessWithExitCode (proc execName execArgs) []

    case exitCode of
        ExitSuccess -> return (Right stdout)
        _           -> return (Left stderr)

execProcessEither :: FilePath -> [String] -> IO (Either String ())
execProcessEither execName execArgs = do
    (exitCode, _, stderr) <- readCreateProcessWithExitCode (proc execName execArgs) []

    case exitCode of
        ExitSuccess -> return (Right ())
        _           -> return (Left stderr)
