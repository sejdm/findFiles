module EitherExtras (MaybeType, maybeToEither) where

import Control.Monad.Except
import Data.Monoid

class MaybeType f where
  maybeToEither :: MonadError e m => e -> f a -> m a

instance MaybeType Maybe where
  maybeToEither e Nothing = throwError e
  maybeToEither _ (Just x) = return x


instance MaybeType First where
  maybeToEither e (First Nothing) = throwError e
  maybeToEither _ (First ( Just x)) = return x

instance MaybeType f => MaybeType (Alt f) where
  maybeToEither e = maybeToEither e . getAlt
