module Control.Either.Extra (throwEither, throwEitherM) where

import           GHC.Stack (HasCallStack)

throwEither :: HasCallStack => Either String a -> a
throwEither = either error id

throwEitherM :: (HasCallStack, Monad m) => m (Either String a) -> m a
throwEitherM = (throwEither <$>)

