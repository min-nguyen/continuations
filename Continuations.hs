{-# LANGUAGE DeriveFunctor, RankNTypes #-}

module Continuations where

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

-- instance Applicative TraceCPS where
--   pure x = TraceCPS { variables = [], output = x, density = 1 }
--   tf <*> tx =
--     TraceCPS
--       { variables = variables tf ++ variables tx,
--         output = output tf (output tx),
--         density = density tf * density tx
--       }

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

-- | Either
data Either' e a = Left' e | Right' a

newtype EitherCPS e a = EitherCPS { unEitherCPS :: forall r. (e -> r) -> (a -> r) -> r }

runEitherCPS :: EitherCPS e a -> Either' e a
runEitherCPS m = (unEitherCPS m) Left' Right'

-- | Writer
newtype Writer' w a = Writer' { runWriter' :: (a, w) }

newtype WriterCPS w a = WriterCPS { unWriterCPS :: forall r. (a -> w -> r) -> r}

runWriterCPS :: Monoid w => WriterCPS w a -> (a, w)
runWriterCPS m = (unWriterCPS m) (\a w -> (a, w))

-- | Reader
newtype Reader' env a = Reader' { runReader' :: env -> a }

newtype ReaderCPS env a = ReaderCPS { unReaderCPS :: forall r. env -> (a -> r) -> r }

runReaderCPS :: Monoid env => ReaderCPS env a -> env -> a
runReaderCPS m env = (unReaderCPS m) env (\a -> a)

-- | State
newtype State' s a = State' { runState' :: s -> (a, s) }

newtype StateCPS s a = StateCPS { unStateCPS :: forall r. s -> (a -> s -> r) -> r }

runStateCPS :: StateCPS s a -> s -> (a, s)
runStateCPS m s = (unStateCPS m) s (\a s' -> (a, s))

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