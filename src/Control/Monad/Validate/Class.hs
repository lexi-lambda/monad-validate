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
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Except
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader

{-| The class of validation monads, intended to be used to validate data structures while collecting
errors along the way. In a sense, 'MonadValidate' is like a combination of
'Control.Monad.Error.Class.MonadError' and 'Control.Monad.Writer.Class.MonadWriter', but it isn’t
entirely like either. The two essential differences are:

  1. Unlike 'Control.Monad.Error.Class.throwError', raising an error using 'refute' does not always
     abort the entire computation—it may only abort a local part of it.

  2. Unlike 'Control.Monad.Writer.Class.tell', raising an error using 'dispute' still causes the
     computation to globally fail, it just doesn’t affect local execution.

For a more thorough explanation, with examples, see the documentation for
'Control.Monad.Validate.ValidateT'.
-}
class (Monad m, Semigroup e) => MonadValidate e m | m -> e where
  -- | Raises a fatal validation error. Aborts the current branch of the validation (i.e. does not
  -- return).
  refute :: e -> m a

  -- | Raises a non-fatal validation error. The overall validation fails, and the error is recorded,
  -- but validation continues in an attempt to try and discover more errors.
  dispute :: e -> m ()

  default refute :: (MonadTrans t, MonadValidate e m', m ~ t m') => e -> m a
  refute = lift . refute
  default dispute :: (MonadTrans t, MonadValidate e m', m ~ t m') => e -> m ()
  dispute = lift . dispute

instance (MonadValidate e m) => MonadValidate e (ContT r m)
instance (MonadValidate e m) => MonadValidate e (ExceptT a m)
instance (MonadValidate e m) => MonadValidate e (IdentityT m)
instance (MonadValidate e m) => MonadValidate e (MaybeT m)
instance (MonadValidate e m) => MonadValidate e (ReaderT r m)
instance (MonadValidate e m) => MonadValidate e (CPS.RWST r w s m)
instance (MonadValidate e m, Monoid w) => MonadValidate e (Lazy.RWST r w s m)
instance (MonadValidate e m, Monoid w) => MonadValidate e (Strict.RWST r w s m)
instance (MonadValidate e m) => MonadValidate e (Lazy.StateT s m)
instance (MonadValidate e m) => MonadValidate e (Strict.StateT s m)
instance (MonadValidate e m) => MonadValidate e (CPS.WriterT w m)
instance (MonadValidate e m, Monoid w) => MonadValidate e (Lazy.WriterT w m)
instance (MonadValidate e m, Monoid w) => MonadValidate e (Strict.WriterT w m)
