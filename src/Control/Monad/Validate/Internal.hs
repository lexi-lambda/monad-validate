{-# OPTIONS_HADDOCK not-home #-}
{-# LANGUAGE UndecidableInstances #-}

-- | __This is an internal module.__ Backwards compatibility will not be maintained. See
-- "Control.Monad.Validate" for the public interface.
module Control.Monad.Validate.Internal where

import Control.Monad.IO.Class
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Reader.Class
import Control.Monad.State.Strict
import Control.Monad.Trans.Control
import Control.Monad.Writer.Class
import Data.Functor
import Data.Functor.Identity
import Data.Tuple (swap)
import GHC.Stack (HasCallStack)

import Control.Monad.Validate.Class

{-| 'ValidateT' is a monad transformer for writing validations. Like 'ExceptT', 'ValidateT' is
primarily concerned with the production of errors, but it differs from 'ExceptT' in that 'ValidateT'
is designed not to necessarily halt on the first error. Instead, it provides a mechanism for
collecting many warnings or errors, ideally as many as possible, before failing. In that sense,
'ValidateT' is also somewhat like 'Control.Monad.Writer.WriterT', but it is not /just/ a combination
of 'ExceptT' and 'Control.Monad.Writer.WriterT'. Specifically, it differs in the following two
respects:

  1. 'ValidateT' automatically collects errors from all branches of an 'Applicative' expression,
     making it possible to write code in the same style that one would use with 'ExceptT' and
     automatically get additional information for free. (This is especially true when used in
     combination with the @ApplicativeDo@ language extension.)

  2. 'ValidateT' provides error signaling operators, 'refute' and 'dispute', which are similar to
     'throwError' and 'tell', respectively. However, both operators combine raised errors into a
     single value (using an arbitrary 'Semigroup'), so the relative ordering of validation errors is
     properly respected. (Of course, if the order doesn’t matter to you, you can choose to
     accumulate errors into an unordered container.)

== An introduction to 'ValidateT'

The first of the above two points is by far the most interesting feature of 'ValidateT'. Let’s make
it more concrete with an example:

@
>>> 'runValidate' ('refute' ["bang"] '*>' 'refute' ["boom"])
'Left' ["bang", "boom"]
@

At first blush, the above example may lead you to believe that 'refute' is like 'tell' from
'Control.Monad.Writer.WriterT', but it is actually more like 'throwError'. Consider its type:

@
'refute' :: 'MonadValidate' e m => e -> m a
@

Note that, like 'throwError', 'refute' is polymorphic in its return type, which is to say it never
returns. Indeed, if we introduce a dependency on a computation that fails using 'refute' via
'>>=', the downstream computation will not be run:

@
>>> let getString = 'refute' ["bang"] '*>' 'pure' "boom"
        useString a = 'refute' [a]
    in 'runValidate' (getString '>>=' useString)
'Left' ["bang"]
@

This works because although the 'Monad' instance for 'ValidateT' fails as soon as the first 'refute'
is executed (as it must due to the way the second argument of '>>=' depends on the result of its
first argument), the 'Applicative' instance runs all branches of '<*>' and combines the errors
produced by all of them. When @ApplicativeDo@ is enabled, this can lead to some “magical” looking
error reporting where validation automatically continues on each sub-piece of a piece of data until
it absolutely cannot proceed any further. As an example, this package’s test suite includes the
following function:

@
validateQueryRequest :: ('MonadReader' Env m, 'MonadValidate' [Error] m) => Value -> m QueryRequest
validateQueryRequest req = withObject "request" req '$' \o -> do
  qrAuth           <- withKey o "auth_token" parseAuthToken
  ~(qrTable, info) <- withKey o "table" parseTableName
  qrQuery          <- withKey o "query" parseQuery
  'Data.Foldable.for_' info '$' \tableInfo -> pushPath "query" '$'
    validateQuery qrTable tableInfo (atIsAdmin qrAuth) qrQuery
  'pure' QueryRequest { qrAuth, qrTable, qrQuery }
@

The above @do@ block parses and validates some JSON, and it’s written as straight line code, but
with @ApplicativeDo@ enabled (along with the @-foptimal-applicative-do@ option, which makes GHC try
a little harder), it still produces errors for all parts of the input document at once:

@
>>> 'flip' 'Control.Monad.Reader.runReader' env '.' 'runValidateT' '$' validateQueryRequest [aesonQQ|
      { "auth_token": 123
      , "table": { "name": "users" }
      , "query": { "add":
        [ { "lit": "42" }
        , { "select": "points" } ]}
      }|]
'Left' [ Error ["auth_token"] (JSONBadValue "string" (Number 123))
     , Error ["table"] (JSONMissingKey "schema")
     , Error ["query", "add", "lit"] (JSONBadValue "number" (String "42")) ]
@

The penultimate statement in the @do@ block—the one with the call to @validateQuery@—depends on
several of the bindings bound earlier in the same @do@ block, namely @qrAuth@, @info@, and
@qrQuery@. Because of that, @validateQuery@ will not be executed so long as any of its dependencies
fail. As soon as they all succeed, their results will be passed to @validateQuery@ as usual, and
validation will continue.

== The full details

Although 'ValidateT' (with @ApplicativeDo@) may seem magical, of course, it is not. As alluded to
above, 'ValidateT' simply provides a '<*>' implementation that collects errors produced by both
arguments rather than short-circuiting as soon as the first error is raised.

However, that explanation alone may raise some additional questions. What about the monad laws? When
'ValidateT' is used in a monad transformer stack, what happens to side effects? And what are
'ValidateT'’s performance characteristics? The remainder of this section discusses those topics.

=== 'ValidateT' and the 'Monad' laws

'ValidateT'’s 'Applicative' and 'Monad' instances do not conform to a strict interpretation of the
'Monad' laws, which dictate that '<*>' must be equivalent to 'ap'. For 'ValidateT', this is not true
if we consider “equivalent” to mean '=='. However, if we accept a slightly weaker notion of
equivalence, we can satisfy the laws. Specifically, we may use the definition that some 'Validate'
action @a@ is equivalent to another action @b@ iff

  * if @'runValidate' a@ produces @'Right' x@, then @'runValidate' b@ must produce @'Right' y@ where
    @x '==' y@ (and '==' is the usual Haskell '=='),

  * and if @'runValidate' a@ produces @'Left' x@, then @'runValidate' b@ must produce @'Left' y@
    (but @x@ and @y@ may be unrelated).

In other words, our definition of equivalence is like '==', except that we make no guarantees about
the /contents/ of an error should one occur. However, we /do/ guarantee that replacing '<*>' with
'ap' or vice versa will never change an error to a success or a success to an error, nor will it
change the value of a successful result in any way. To put it another way, 'ValidateT' provides
“best effort” error reporting: it will never return fewer errors than an equivalent use of
'ExceptT', but it might return more.

=== Using 'ValidateT' with other monad transformers

'ValidateT' is a valid, lawful, generally well-behaved monad transformer, and it is safe to use
within a larger monad transformer stack. Instances for the most common @mtl@-style typeclasses are
provided. __However__, be warned: many common monad transformers do not have sufficiently
order-independent 'Applicative' instances for 'ValidateT'’s 'Applicative' instance to actually
collect errors from multiple branches of a computation.

To understand why that might be, consider that 'StateT' must enforce a left-to-right evaluation
order for '<*>' in order to thread the state through the computation. If the @a@ action in an
expression @a '<*>' b@ fails, then it is simply not possible to run @b@ since @b@ may still depend
on the state that would have been produced by @a@. Similarly, 'ExceptT' enforces a left-to-right
evaluation because it aborts a computation as soon as an error is thrown. Using 'ValidateT' with
these kinds of monad transformers will cause it to effectively degrade to
'Control.Monad.Writer.WriterT' over 'ExceptT' since it will not be able to gather any errors
produced by 'refute' beyond the first one.

However, even that isn’t the whole story, since the relative order of monads in a monad transformer
stack can affect things further. For example, while the 'StateT' monad transformer enforces
left-to-right evaluation order, it only does this for the monad /underneath/ it, so although
@'StateT' s ('ValidateT' e)@ will not be able to collect multiple errors, @'ValidateT' e
('State' s)@ will. Note, however, that those two types differ in other ways, too—running each to
completion results in different types:

@
'runState' ('runValidateT' m) s :: ('Either' e a, s)
'runValidate' ('runStateT' m s) :: 'Either' e (a, s)
@

That kind of difference is generally true when using monad transformers—the two combinations of
'ExceptT' and 'StateT' have the same types as above, for example—but because 'ValidateT' needs to be
on top of certain transformers for it to be useful, combining 'ValidateT' with certain transformers
may be of little practical use.

One way to identify which monad transformers are uncooperative in the aforementioned way is to look
at the constraints included in the context of the transformer’s 'Applicative' instance. Transformers
like 'Control.Monad.State.StateT' have instances of the shape

@
instance 'Monad' m => 'Applicative' ('StateT' s m)
@

which notably require 'Monad' instances just to implement 'Applicative'! However, this is not always
sufficient for distinguishing which functions or instances use '<*>' and which use '>>=', especially
since many older libraries (which predate 'Applicative') may include 'Monad' contraints even when
they only use features of 'Applicative'. The only way to be certain is to examine the
implementation (or conservatively write code that is explicitly restricted to 'Applicative').

(As it happens, 'ValidateT'’s 'Applicative' is actually one such “uncooperative” instance itself: it
has a 'Monad' constraint in its context. It is possible to write an implementation of 'ValidateT'
without that constraint, but its '<*>' would necessarily leak space in the same way
'Control.Monad.Writer.WriterT'’s '>>=' leaks space. If you have a reason to want the less efficient
but more permissive variant, please let the author of this library know, as she would probably find
it interesting.)

== Performance characteristics of 'ValidateT'

Although the interface to 'ValidateT' is minimal, there are surprisingly many different ways to
implement it, each with its own set of performance tradeoffs. Here is a quick summary of the choices
'ValidateT' makes:

  1. 'ValidateT' is __strict__ in the set of errors it accumulates, which is to say it reduces them
     to weak head normal form (WHNF) via 'seq' immediately upon any call to 'refute' or 'dispute'.

  2. Furthermore, all of 'ValidateT'’s operations, including '<*>', operate in __constant space__.
     This means, for example, that evaluating @'sequence_' xs@ will consume constant space
     regardless of the size of @xs@, not counting any space consumed purely due to the relevant
     'Foldable' instance’s traversal of @xs@.

  3. Finally, 'ValidateT' accumulates errors in a __left-associative__ manner, which is to say that
     any uses of 'refute' or 'dispute' combine the existing set of errors, @e@, with the added set
     of errors, @e'@, via the expression @e '<>' e'@.

A good rule of thumb is that 'ValidateT' has similar performance characteristics to
@'Data.Foldable.foldl'' ('<>')@, while types like @Validation@ from the @either@ package tend to
have similar performance characteristics to @'foldr' ('<>')@. That decision has both significant
advantages and significant disadvantages; the following subsections elaborate further.

=== '<*>' takes constant space

Great care has been taken in the implementation of '<*>' to ensure it does not leak space. Notably,
the same /cannot/ be said for many existing implementations of similar concepts. For example, you
will find that executing the expression

@
let m () = 'pure' () '*>' m () in m ()
@

may continuously allocate memory until it is exhausted for types such as @Validation@ (from the
@either@ package), but 'ValidateT' will execute it in constant space. This point may seem silly,
since the above definition of @m ()@ will never do anything useful, anyway, but the same point also
applies to operations like 'sequence_'.

In practice, this issue matters far less for types like @Validation@ than it does for 'ValidateT',
as @Validation@ and its cousins don’t have a 'Monad' instance and do not generally experience the
same usage patterns. (The additional laziness they are capable of can sometimes even avoid the space
leak altogether.) However, it can be relevant more often for 'ValidateT', so this implementation
makes choices to avoid the potential for the leak altogether.

=== Errors are accumulated using strict, left-associated '<>'

A major consequence of the decision to both strictly accumulate state and maintain constant space is
that 'ValidateT'’s internal applications of '<>' to combine errors are naturally strict and
left-associated, not lazy and right-associated like they are for types like @Validation@. If the
number of errors your validation generates is small, this difference is irrelevant, but if it is
large, the difference in association can prove disastrous if the 'Semigroup' you choose to
accumulate errors in is @[a]@!

To make it painfully explicit why using @[a]@ can come back to bite you, consider that each time
'ValidateT' executes @'refute' e'@, given some existing collection of errors @e@, it (strictly)
evalutes @e '<>' e'@ to obtain a new collection of errors. Now consider the implications of that
if @e@ is a ten thousand element list: '<>' will have to traverse /all/ ten thousand elements and
reallocate a fresh cons cell for every single one in order to build the new list, even if just one
element is being appended to the end! Unfortunately, the ubiquitous, built-in @[a]@ type is clearly
an exceptionally poor choice for this pattern of accumulation.

Fortunately, the solution is quite simple: use a different data structure. If order doesn’t matter,
use a @Set@ or @HashSet@. If it does, but either LIFO consumption of the data is okay or you are
okay with paying to reverse the data once after collecting the errors, @'Data.Semigroup.Dual' [a]@
to accumulate elements in an efficient manner. If neither is true, use a data structure like @Seq@
that provides an efficient implementation of a functional queue. You can always convert back to a
plain list at the end once you’re done, if you have to. -}
newtype ValidateT e m a = ValidateT
  { getValidateT :: forall s. StateT (MonoMaybe s e) (ExceptT e m) a }
-- Sadly, GeneralizedNewtypeDeriving can’t help us here due to the inner forall, but we can at least
-- derive the Functor instance.
deriving instance (Functor m) => Functor (ValidateT e m)

validateT
  :: forall e m a. (Functor m)
  => (forall s. MonoMaybe s e -> m (Either e (MonoMaybe s e, a)))
  -> ValidateT e m a
validateT f = ValidateT (StateT (ExceptT . (fmap (fmap swap) . f)))
{-# INLINE validateT #-}

unValidateT
  :: forall s e m a. (Functor m)
  => MonoMaybe s e -> ValidateT e m a -> m (Either e (MonoMaybe s e, a))
unValidateT e (ValidateT m) = runExceptT (swap <$> runStateT m e)
{-# INLINE unValidateT #-}

instance (Monad m) => Applicative (ValidateT e m) where
  pure v = ValidateT (pure v)
  {-# INLINE pure #-}

  m1 <*> m2 = validateT $ \e0 ->
    unValidateT e0 m1 >>= \case
      Left e1 -> unValidateT (MJust @'SJust e1) m2 <&> \case
        Left e2 -> Left e2
        Right (MJust e2, _) -> Left e2
      Right (e1, v1) -> unValidateT e1 m2 <&> \case
        Left e2 -> Left e2
        Right (e2, v2) -> Right (e2, v1 v2)
  {-# INLINABLE (<*>) #-}

instance (Monad m) => Monad (ValidateT e m) where
  ValidateT x >>= f = ValidateT (x >>= (getValidateT . f))
  {-# INLINE (>>=) #-}

instance MonadTrans (ValidateT e) where
  lift m = ValidateT (lift $ lift m)
  {-# INLINE lift #-}

instance (MonadIO m) => MonadIO (ValidateT e m) where
  liftIO = lift . liftIO
  {-# INLINE liftIO #-}

instance (MonadBase b m) => MonadBase b (ValidateT e m) where
  liftBase = lift . liftBase
  {-# INLINE liftBase #-}

-- | An opaque type used to capture the current state of a 'ValidateT' computation, used as the
-- 'StT' instance for 'ValidateT'. It is opaque in an attempt to protect internal invariants about
-- the state, but it is unfortunately still theoretically possible for it to be misused (but such
-- misuses are exceedingly unlikely).
data ValidateTState e a = forall s. ValidateTState
  { getValidateTState :: Either e (MonoMaybe s e, a) }
deriving instance (Show e, Show a) => Show (ValidateTState e a)
deriving instance Functor (ValidateTState e)

instance MonadTransControl (ValidateT e) where
  type StT (ValidateT e) a = ValidateTState e a

  liftWith f = validateT $ \e ->
    Right . (e,) <$> f (fmap ValidateTState . unValidateT e)
  {-# INLINABLE liftWith #-}

  restoreT :: (HasCallStack, Monad m) => m (StT (ValidateT e) a) -> ValidateT e m a
  restoreT m = validateT $ \e1 -> do
    ValidateTState r <- m
    case e1 of
      MNothing -> case r of
        Left e2             -> pure $ Left e2
        Right (MJust e2, v) -> pure $ Right (MJust e2, v)
        Right (MNothing, v) -> pure $ Right (MNothing, v)
      MJust _ -> case r of
        Left e2             -> pure $ Left e2
        Right (MJust e2, v) -> pure $ Right (MJust e2, v)
        Right (MNothing, _) -> error
          $  "Control.Monad.Validate.ValidateT#restoreT: panic!\n"
          <> "  An attempt was made to restore from a state captured before any validation\n"
          <> "  errors occurred into a context with validation errors. This is probably the\n"
          <> "  result of an incorrect use of MonadBaseControl (as validation errors should\n"
          <> "  strictly increase). Ensure that all state is restored immediately upon\n"
          <> "  returning from the base monad (or is not restored at all).\n"
          <> "\n"
          <> "  If you believe your use of MonadBaseControl is not in error, and this is a bug\n"
          <> "  in ValidateT, please submit a bug report."
  {-# INLINABLE restoreT #-}

instance (MonadBaseControl b m) => MonadBaseControl b (ValidateT e m) where
  type StM (ValidateT e m) a = ComposeSt (ValidateT e) m a
  liftBaseWith = defaultLiftBaseWith
  restoreM = defaultRestoreM
  {-# INLINE liftBaseWith #-}
  {-# INLINE restoreM #-}

liftCatch
  :: (Functor m)
  => (forall b. m b -> (e -> m b) -> m b)
  -> ValidateT d m a -> (e -> ValidateT d m a) -> ValidateT d m a
liftCatch catchE m f = validateT $ \e ->
  catchE (unValidateT e m) (unValidateT e . f)
{-# INLINE liftCatch #-}

instance (MonadError e m) => MonadError e (ValidateT a m) where
  throwError = lift . throwError
  catchError = liftCatch catchError
  {-# INLINE throwError #-}
  {-# INLINE catchError #-}

instance (MonadReader r m) => MonadReader r (ValidateT e m) where
  ask = lift ask
  local f (ValidateT m) = ValidateT (local f m)
  reader = lift . reader
  {-# INLINE ask #-}
  {-# INLINE local #-}
  {-# INLINE reader #-}

instance (MonadState s m) => MonadState s (ValidateT e m) where
  get = lift get
  put = lift . put
  state = lift . state
  {-# INLINE get #-}
  {-# INLINE put #-}
  {-# INLINE state #-}

instance (MonadWriter w m) => MonadWriter w (ValidateT e m) where
  writer = lift . writer
  tell = lift . tell
  listen (ValidateT m) = ValidateT (listen m)
  pass (ValidateT m) = ValidateT (pass m)
  {-# INLINE writer #-}
  {-# INLINE tell #-}
  {-# INLINE listen #-}
  {-# INLINE pass #-}

instance (MonadThrow m) => MonadThrow (ValidateT e m) where
  throwM = lift . throwM
  {-# INLINE throwM #-}

instance (MonadCatch m) => MonadCatch (ValidateT e m) where
  catch = liftCatch catch
  {-# INLINE catch #-}

liftMask
  :: (Functor m)
  => (forall c. ((forall a. m a -> m a) -> m c) -> m c)
  -> ((forall a. ValidateT e m a -> ValidateT e m a) -> ValidateT e m b) -> ValidateT e m b
liftMask maskE f = validateT $ \e1 ->
  maskE $ \unmask ->
    unValidateT e1 $ f $ \m ->
      validateT $ \e2 ->
        unmask $ unValidateT e2 m
{-# INLINE liftMask #-}

instance (MonadMask m) => MonadMask (ValidateT e m) where
  mask = liftMask mask
  uninterruptibleMask = liftMask uninterruptibleMask
  generalBracket m f g = ValidateT $ generalBracket
    (getValidateT m)
    (\a b -> getValidateT $ f a b)
    (\a -> getValidateT $ g a)
  {-# INLINE mask #-}
  {-# INLINE uninterruptibleMask #-}
  {-# INLINE generalBracket #-}

instance (Monad m, Semigroup e) => MonadValidate e (ValidateT e m) where
  refute e2 = validateT $ \e1 ->
    let !e3 = monoMaybe e2 (<> e2) e1 in pure (Left e3)
  dispute e2 = validateT $ \e1 ->
    let !e3 = monoMaybe e2 (<> e2) e1 in pure (Right (MJust e3, ()))
  tolerate m = validateT $ \e1 ->
    Right . either (\e2 -> (MJust e2, Nothing)) (fmap Just) <$> unValidateT e1 m
  {-# INLINABLE refute #-}
  {-# INLINABLE dispute #-}
  {-# INLINABLE tolerate #-}

-- | Runs a 'ValidateT' computation, returning the errors raised by 'refute' or 'dispute' if any,
-- otherwise returning the computation’s result.
runValidateT :: forall e m a. (Functor m) => ValidateT e m a -> m (Either e a)
runValidateT m = unValidateT MNothing m <&> \case
  Left e              -> Left e
  Right (MJust e, _)  -> Left e
  Right (MNothing, v) -> Right v

-- | Runs a 'ValidateT' computation, returning the errors on failure or 'mempty' on success. The
-- computation’s result, if any, is discarded.
--
-- @
-- >>> 'execValidate' ('refute' ["bang"])
-- ["bang"]
-- >>> 'execValidate' @[] ('pure' 42)
-- []
-- @
execValidateT :: forall e m a. (Monoid e, Functor m) => ValidateT e m a -> m e
execValidateT = fmap (either id mempty) . runValidateT

{-| Runs a 'ValidateT' transformer by interpreting it in an underlying transformer with a
'MonadValidate' instance. That might seem like a strange thing to do, but it can be useful in
combination with 'mapErrors' to locally alter the error type in a larger 'ValidateT' computation.
For example:

@
throwsIntegers :: 'MonadValidate' ['Integer'] m => m ()
throwsIntegers = 'dispute' [42]

throwsBools :: 'MonadValidate' ['Bool'] m => m ()
throwsBools = 'dispute' ['False']

throwsBoth :: 'MonadValidate' ['Either' 'Integer' 'Bool'] m => m ()
throwsBoth = do
  'embedValidateT' '$' 'mapErrors' ('map' 'Left') throwsIntegers
  'embedValidateT' '$' 'mapErrors' ('map' 'Right') throwsBools

>>> 'runValidate' throwsBoth
'Left' ['Left' 42, 'Right' False]
@

@since 1.1.0.0 -}
embedValidateT :: forall e m a. (MonadValidate e m) => ValidateT e m a -> m a
embedValidateT m = unValidateT MNothing m >>= \case
  Left e              -> refute e
  Right (MJust e, v)  -> dispute e $> v
  Right (MNothing, v) -> pure v

-- | Applies a function to all validation errors produced by a 'ValidateT' computation.
--
-- @
-- >>> 'runValidate' '$' 'mapErrors' ('map' 'show') ('refute' [11, 42])
-- 'Left' ["11", "42"]
-- @
--
-- @since 1.1.0.0
mapErrors
  :: forall e e' m a. (Monad m, Semigroup e')
  => (e -> e') -> ValidateT e m a -> ValidateT e' m a
mapErrors f m = lift (unValidateT MNothing m) >>= \case
  Left e              -> refute (f e)
  Right (MJust e, v)  -> dispute (f e) $> v
  Right (MNothing, v) -> pure v

-- | 'ValidateT' specialized to the 'Identity' base monad. See 'ValidateT' for usage information.
type Validate e = ValidateT e Identity

-- | See 'runValidateT'.
runValidate :: forall e a. Validate e a -> Either e a
runValidate = runIdentity . runValidateT
{-# INLINE runValidate #-}

-- | See 'execValidateT'.
execValidate :: forall e a. (Monoid e) => Validate e a -> e
execValidate = runIdentity . execValidateT
{-# INLINE execValidate #-}

{-| Monotonically increasing 'Maybe' values. A function with the type

@
forall s. 'MonoMaybe' s Foo -> 'MonoMaybe' s Bar
@

may return 'MNothing' only when given 'MNothing', but it may return 'MJust' for any input. This
is useful for keeping track of the error state within 'ValidateT', since we want to statically
prevent the possibility of a 'ValidateT' action being passed a nonempty set of errors but returning
no errors.

The benefit of this additional type tracking shows up most prominently in the implementation of
'<*>'. Consider an expression @x '<*>' y@, where @x@ is an action that fails, but @y@ is an action
that succeeds. We pass the errors returned by @x@ to @y@, then pattern-match on @y@’s result. If @y@
succeeds, we’ll end up with a tuple of type @('MonoMaybe' ''SJust' e, a)@. We can’t use the second
element of that tuple at all because we need to return a value of type @b@, but the only way to get
one is to apply a function of type @a -> b@ returned by @x@… which we don’t have, since @x@ failed.

Since we can’t produce a value of type @'Right' b@, our only option is to return a value of type
@'Left' e@. But if the first element of the tuple had type @'Maybe' e@, we’d now be in a sticky
situation! Its value could be 'Nothing', but we need it to be @'Just' e@ since we only have a
'Semigroup' instance for @e@, not a 'Monoid' instance, so we can’t produce an @e@ out of thin air.
However, by returning a 'MonoMaybe', we guarantee that the result will be @'MJust' e@, and we can
proceed safely.
-}
data MonoMaybe s a where
  MNothing :: MonoMaybe 'SMaybe a
  MJust :: forall s a. !a -> MonoMaybe s a
deriving instance (Show a) => Show (MonoMaybe s a)
deriving instance (Eq a) => Eq (MonoMaybe s a)
deriving instance (Ord a) => Ord (MonoMaybe s a)
deriving instance Functor (MonoMaybe s)

-- | The kind of types used to track the current state of a 'MonoMaybe' value.
data MonoMaybeS = SMaybe | SJust

-- | Like 'maybe' but for 'MonoMaybe'.
monoMaybe :: (s ~ 'SMaybe => b) -> (a -> b) -> MonoMaybe s a -> b
monoMaybe v f = \case
  MNothing -> v
  MJust x  -> f x
{-# INLINE monoMaybe #-}
