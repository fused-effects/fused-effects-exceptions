{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, RankNTypes, TypeOperators, UndecidableInstances #-}

{- | A carrier for the 'State' effect. It uses an 'IORef' internally to handle its state, and thus is safe to use with "Control.Carrier.Resource". Underlying 'IORef' operations are performed with 'readIORef' and 'writeIORef'.

Note that the parameter order in 'runState', 'evalState', and 'execState' is reversed compared the equivalent functions provided by @transformers@. This is an intentional decision made to enable the composition of effect handlers with '.' without invoking 'flip'.
-}
module Control.Carrier.State.IORef
( -- * State effect
  module Control.Effect.State
  -- * Strict state carrier
, runState
, evalState
, execState
, StateC(..)
  -- * Re-exports
, Carrier
, run
) where

import           Control.Applicative       (Alternative (..))
import           Control.Carrier
import           Control.Carrier.Reader
import           Control.Effect.State
import           Control.Monad             (MonadPlus (..))
import qualified Control.Monad.Fail        as Fail
import           Control.Monad.Fix
import           Control.Monad.IO.Class
import           Control.Monad.IO.Unlift
import           Control.Monad.Trans.Class
import           Data.IORef

-- | Run a 'State' effect starting from the passed value.
--
--   prop> run (runState a (pure b)) === (a, b)
--
-- @since 1.0.0.0
runState :: MonadIO m => s -> StateC s m a -> m (s, a)
runState s x = do
  ref <- liftIO $ newIORef s
  result <- runReader ref . runStateC $ x
  final <- liftIO . readIORef $ ref
  pure (final, result)
{-# INLINE[3] runState #-}

-- | Run a 'State' effect, yielding the result value and discarding the final state.
--
--   prop> run (evalState a (pure b)) === b
--
-- @since 1.0.0.0
evalState :: forall s m a . MonadIO m => s -> StateC s m a -> m a
evalState s x = do
  ref <- liftIO $ newIORef s
  runReader ref . runStateC $ x
{-# INLINE[3] evalState #-}

-- | Run a 'State' effect, yielding the final state and discarding the return value.
--
--   prop> run (execState a (pure b)) === a
--
-- @since 1.0.0.0
execState :: forall s m a . MonadIO m => s -> StateC s m a -> m s
execState s = fmap fst . runState s
{-# INLINE[3] execState #-}

-- | @since 1.0.0.0
newtype StateC s m a = StateC { runStateC :: ReaderC (IORef s) m a }
  deriving (Alternative, Applicative, Functor, Monad, Fail.MonadFail, MonadFix, MonadIO, MonadPlus)

instance MonadUnliftIO m => MonadUnliftIO (StateC s m) where
  askUnliftIO = StateC . ReaderC $ \r -> withUnliftIO $ \u -> pure (UnliftIO (\(StateC (ReaderC x)) -> unliftIO u (x r)))
  {-# INLINE askUnliftIO #-}
  withRunInIO inner = StateC . ReaderC $ \r -> withRunInIO $ \go -> inner (go . runReader r . runStateC)
  {-# INLINE withRunInIO #-}

instance (MonadIO m, Carrier sig m, Effect sig) => Carrier (State s :+: sig) (StateC s m) where
  eff (L act) = do
    ref <- StateC ask
    case act of
      Put s k -> liftIO (writeIORef ref s) *> k
      Get k   -> liftIO (readIORef ref) >>= k
  eff (R other) = StateC (eff (R (handleCoercible other)))
  {-# INLINE eff #-}
