module Control.Either.Extra (throwEither, throwEitherM) where

import           GHC.Stack (HasCallStack)

throwEither :: HasCallStack => Either String a -> a
throwEither = \case
    Right x     -> x
    Left errMsg -> error errMsg

throwEitherM :: (HasCallStack, Monad m) => m (Either String a) -> m a
throwEitherM = (throwEither <$>)

