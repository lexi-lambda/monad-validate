{-# OPTIONS_HADDOCK not-home #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Validate.Class
  ( MonadValidate(..)
  ) where

import qualified Control.Monad.Trans.RWS.CPS as CPS
import qualified Control.Monad.Trans.RWS.Lazy as Lazy
import qualified Control.Monad.Trans.RWS.Strict as Strict
import qualified Control.Monad.Trans.State.Lazy as Lazy
import qualified Control.Monad.Trans.State.Strict as Strict
import qualified Control.Monad.Trans.Writer.CPS as CPS
import qualified Control.Monad.Trans.Writer.Lazy as Lazy
import qualified Control.Monad.Trans.Writer.Strict as Strict

import Control.Monad.Trans.Class
import Control.Monad.Trans.Control
import Control.Monad.Trans.Except
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Data.Functor

{-| The class of validation monads, intended to be used to validate data structures while collecting
errors along the way. In a sense, 'MonadValidate' is like a combination of
'Control.Monad.Error.Class.MonadError' and 'Control.Monad.Writer.Class.MonadWriter', but it isn’t
entirely like either. The two essential differences are:

  1. Unlike 'Control.Monad.Error.Class.throwError', raising an error using 'refute' does not always
     abort the entire computation—it may only abort a local part of it.

  2. Unlike 'Control.Monad.Writer.Class.tell', raising an error using 'dispute' still causes the
     computation to globally fail, it just doesn’t affect local execution.

For a more thorough explanation, with examples, see the documentation for
'Control.Monad.Validate.ValidateT'. -}
class (Monad m, Semigroup e) => MonadValidate e m | m -> e where
  -- | Raises a fatal validation error. Aborts the current branch of the validation (i.e. does not
  -- return).
  --
  -- @
  -- >>> 'Control.Monad.Validate.runValidate' ('refute' ["boom"] '>>' 'refute' ["bang"])
  -- 'Left' ["boom"]
  -- @
  refute :: e -> m a

  -- | Raises a non-fatal validation error. The overall validation fails, and the error is recorded,
  -- but validation continues in an attempt to try and discover more errors.
  --
  -- @
  -- >>> 'Control.Monad.Validate.runValidate' ('dispute' ["boom"] '>>' 'dispute' ["bang"])
  -- 'Left' ["boom", "bang"]
  -- @
  dispute :: e -> m ()

  -- | @'tolerate' m@ behaves like @m@, except that any fatal errors raised by 'refute' are altered
  -- to non-fatal errors that return 'Nothing'. This allows @m@’s result to be used for further
  -- validation if it succeeds without preventing further validation from occurring upon failure.
  --
  -- @
  -- >>> 'Control.Monad.Validate.runValidate' ('tolerate' ('refute' ["boom"]) '>>' 'refute' ["bang"])
  -- 'Left' ["boom", "bang"]
  -- @
  --
  -- @since 1.1.0.0
  tolerate :: m a -> m (Maybe a)

  default refute :: (MonadTrans t, MonadValidate e m', m ~ t m') => e -> m a
  refute = lift . refute
  default dispute :: (MonadTrans t, MonadValidate e m', m ~ t m') => e -> m ()
  dispute = lift . dispute
  default tolerate :: (MonadTransControl t, MonadValidate e m', m ~ t m') => m a -> m (Maybe a)
  tolerate m = liftWith (\run -> tolerate (run m)) >>=
    maybe (pure Nothing) (fmap Just . restoreT . pure)
  {-# INLINE refute #-}
  {-# INLINE dispute #-}
  {-# INLINE tolerate #-}

instance (MonadValidate e m) => MonadValidate e (ExceptT a m)
instance (MonadValidate e m) => MonadValidate e (IdentityT m)
instance (MonadValidate e m) => MonadValidate e (MaybeT m)
instance (MonadValidate e m) => MonadValidate e (ReaderT r m)
instance (MonadValidate e m, Monoid w) => MonadValidate e (Lazy.RWST r w s m)
instance (MonadValidate e m, Monoid w) => MonadValidate e (Strict.RWST r w s m)
instance (MonadValidate e m) => MonadValidate e (Lazy.StateT s m)
instance (MonadValidate e m) => MonadValidate e (Strict.StateT s m)
instance (MonadValidate e m, Monoid w) => MonadValidate e (Lazy.WriterT w m)
instance (MonadValidate e m, Monoid w) => MonadValidate e (Strict.WriterT w m)

instance (MonadValidate e m, Monoid w) => MonadValidate e (CPS.WriterT w m) where
  tolerate m = CPS.writerT $ tolerate (CPS.runWriterT m) <&>
    maybe (Nothing, mempty) (\(v, w) -> (Just v, w))
  {-# INLINE tolerate #-}
instance (MonadValidate e m, Monoid w) => MonadValidate e (CPS.RWST r w s m) where
  tolerate m = CPS.rwsT $ \r s1 -> tolerate (CPS.runRWST m r s1) <&>
    maybe (Nothing, s1, mempty) (\(v, s2, w) -> (Just v, s2, w))
  {-# INLINE tolerate #-}
