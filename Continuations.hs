{-# LANGUAGE DeriveFunctor, RankNTypes #-}

module Continuations where

import Control.Applicative

-- | Trace
data Trace a
  = Trace
      { variables :: [Double],
        output    :: a,
        density   :: Double
      } deriving Functor

newtype TraceCPS a = TraceCPS { unTraceCPS :: forall r. ([Double] -> a -> Double -> r) -> r } deriving Functor

runTraceCPS :: TraceCPS a -> Trace a
runTraceCPS m = (unTraceCPS m) Trace

instance Applicative TraceCPS where
  pure x = TraceCPS (\k -> k [] x 1)
  (TraceCPS mf) <*> (TraceCPS mx) =
    TraceCPS (\k ->
      mf (\v f d -> mx (\v' x d' -> k (v ++ v') (f x) (d * d') ) ) )

instance Monad TraceCPS where
  return = pure
  (TraceCPS mx) >>= f =
    TraceCPS (\k ->
      mx (\v x d -> (unTraceCPS $ f x) (\v' x' d' -> k (v ++ v') x' (d * d') ) ) )

-- | Maybe
data Maybe' a = Just' a | Nothing' deriving Functor

newtype MaybeCPS a = MaybeCPS { unMaybeCPS :: forall r. (a -> r) -> (() -> r) -> r } deriving Functor

runMaybeCPS :: MaybeCPS a -> Maybe' a
runMaybeCPS m = (unMaybeCPS m) Just' (\() -> Nothing')

-- app :: ((a -> r, () -> r) -> r) -> ((a -> b -> r, () -> r) -> r) -> ((b -> r, () -> r) -> r)

instance Applicative MaybeCPS where
  pure a = MaybeCPS (\k_just k_nothing -> k_just a)
  -- mf        :: forall r. (a -> b -> r) -> r -> r
  -- mx        :: forall r. (a -> r)      -> r -> r
  -- k_just    :: forall r. b -> r
  -- k_nothing :: forall r. r
  -- f         :: (a -> b)
  -- x         :: a
  (MaybeCPS mf) <*> (MaybeCPS mx) =
    MaybeCPS (\k_just k_nothing ->
                mf (\f -> mx (\x -> k_just (f x)) k_nothing) k_nothing)
  -- We need to pass mf & mx two continuations each - one for the just case, and one for the nothing case

instance Monad MaybeCPS where
  return               = pure
  -- mx        :: forall r. (a -> r) -> r -> r
  -- f         :: forall r. (a -> MaybeCPS b)
  -- k_just    :: forall r. b -> r
  -- k_nothing :: forall r. r
  -- x         :: a
  (MaybeCPS mx) >>= f  =
    MaybeCPS (\k_just k_nothing ->
                mx (\x -> unMaybeCPS (f x) k_just k_nothing) k_nothing)

instance Alternative MaybeCPS where
  empty = MaybeCPS (\k_just k_nothing -> k_nothing ())
  (MaybeCPS mx) <|> (MaybeCPS my) =
    MaybeCPS (\k_just k_nothing ->
                mx (\x -> k_just x) (\() -> my (\y -> k_just y) k_nothing))


-- | Either
data Either' e a = Left' e | Right' a deriving Functor

newtype EitherCPS e a = EitherCPS { unEitherCPS :: forall r. (e -> r) -> (a -> r) -> r } deriving Functor

runEitherCPS :: EitherCPS e a -> Either' e a
runEitherCPS m = (unEitherCPS m) Left' Right'

instance Applicative (EitherCPS e) where
  pure x = EitherCPS (\k_left k_right -> k_right x)
  -- mf      :: forall r. (e -> r) -> (a -> b -> r) -> r
  -- mx      :: forall r. (e -> r) -> (a -> r)      -> r
  -- k_left  :: forall r. e -> r
  -- k_right :: forall r. b -> r
  -- f       :: a -> b
  -- x       :: a
  (EitherCPS mf) <*> (EitherCPS mx) =
    EitherCPS (\k_left k_right ->
                 mf k_left (\f -> mx k_left (\x -> k_right (f x))))

instance Monad (EitherCPS e) where
  return = pure
  (EitherCPS mx) >>= f =
    EitherCPS (\k_left k_right ->
                 mx k_left (\x -> unEitherCPS (f x) k_left k_right ))

-- | Writer
newtype Writer' w a = Writer' { runWriter' :: (a, w) } deriving Functor

newtype WriterCPS w a = WriterCPS { unWriterCPS :: forall r. (a -> w -> r) -> r} deriving Functor

runWriterCPS :: Monoid w => WriterCPS w a -> (a, w)
runWriterCPS m = (unWriterCPS m) (\a w -> (a, w))

instance Monoid w => Applicative (WriterCPS w) where
  pure x = WriterCPS (\k -> k x mempty)
  (WriterCPS mf) <*> (WriterCPS mx) =
    WriterCPS (\k ->
      mf (\f w1 -> mx (\x w2 -> k (f x) (w1 `mappend` w2))))

instance Monoid w => Monad (WriterCPS w) where
  return = pure
  (WriterCPS mx) >>= f =
    WriterCPS (\k ->
      mx (\x w1 -> unWriterCPS (f x) (\x' w2 -> k x' (w1 `mappend` w2))))

-- | Reader
newtype Reader' env a = Reader' { runReader' :: env -> a } deriving Functor

newtype ReaderCPS env a = ReaderCPS { unReaderCPS :: forall r. env -> (a -> r) -> r } deriving Functor

runReaderCPS :: Monoid env => ReaderCPS env a -> env -> a
runReaderCPS m env = (unReaderCPS m) env (\a -> a)

instance Applicative (ReaderCPS env) where
  pure x = ReaderCPS (\env k -> k x)
  (ReaderCPS mf) <*> (ReaderCPS mx) =
    ReaderCPS (\env k ->
      mf env (\f -> mx env (\x -> k (f x) ) ) )

instance Monad (ReaderCPS env) where
  return = pure
  (ReaderCPS mx) >>= f =
    ReaderCPS (\env k ->
      mx env (\x -> (unReaderCPS $ f x) env k) )

-- | State
newtype State' s a = State' { runState' :: s -> (a, s) } deriving Functor

newtype StateCPS s a = StateCPS { unStateCPS :: forall r. s -> (a -> s -> r) -> r } deriving Functor

runStateCPS :: StateCPS s a -> s -> (a, s)
runStateCPS m s = (unStateCPS m) s (\a s' -> (a, s))

instance Applicative (StateCPS s) where
  pure x = StateCPS (\s k -> k x s)
  (StateCPS mf) <*> (StateCPS mx) =
    StateCPS (\s k -> mf s (\f s' -> mx s' (\x s'' -> k (f x) s'')))

instance Monad (StateCPS s) where
  return = pure
  (StateCPS mx) >>= f =
    StateCPS (\s k -> mx s (\x s' -> (unStateCPS $ f x) s' k))

-- | MaybeT
newtype MaybeT' m a = MaybeT' { runMaybeT :: m (Maybe' a) }

newtype MaybeTCPS m a = MaybeTCPS { unMaybeTCPS :: forall r. (a -> m r) -> (() -> m r) -> m r }
-- or
-- newtype MaybeTCPS m a = MaybeTCPS { unMaybeTCPS :: forall r. (a -> m r) -> m r -> m r }

runMaybeTCPS :: Monad m => MaybeTCPS m a -> m (Maybe' a)
runMaybeTCPS m = (unMaybeTCPS m) (return . Just') (return . (\() -> Nothing'))
-- or
-- runMaybeTCPS m = (unMaybeTCPS m) (return . Just') (return Nothing)

-- | EitherT
newtype EitherT e m a = EitherT { runEitherT :: m (Either' e a) }

newtype EitherTCPS e m a = EitherTCPS { unEitherTCPS :: forall r. (e -> m r) -> (a -> m r) -> m r }

runEitherTCPS :: Monad m => EitherTCPS e m a -> m (Either' e a)
runEitherTCPS m = (unEitherTCPS m) (return . Left') (return . Right')

-- | WriterT
newtype WriterT w m a = Writer { runWriterT :: m (a, w) }

newtype WriterTCPS w m a = WriterTCPS { unWriterTCPS :: forall r. (a -> w -> m r) -> m r}

runWriterTCPS :: Monad m => WriterTCPS w m a -> m (a, w)
runWriterTCPS m = (unWriterTCPS m) (\a w -> return (a, w))

-- | ReaderT
newtype ReaderT env m a = Reader { runReaderT :: env -> m a }

newtype ReaderTCPS env m a = ReaderTCPS { unReaderTCPS :: forall r. env -> (a -> m r) -> m r }

runReaderTCPS :: (Monoid env, Monad m) => ReaderTCPS env m a -> env -> m a
runReaderTCPS m env = (unReaderTCPS m) env (\a -> return a)

-- | StateT
newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }

newtype StateTCPS s m a = StateTCPS { unStateTCPS :: forall r. s -> (a -> s -> m r) -> m r }

runStateTCPS :: Monad m => StateTCPS s m a -> s -> m (a, s)
runStateTCPS (StateTCPS f) s = f s (\x s' -> return (x, s))