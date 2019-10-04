{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, RankNTypes, TypeOperators, UndecidableInstances #-}
module Control.Carrier.Catch
( -- * Catch effect
  module Control.Effect.Catch
  -- * Catch carrier
, runCatch
, CatchC(..)
  -- * Re-exports
, Carrier
, run
) where

import           Control.Carrier
import           Control.Carrier.Reader
import           Control.Effect.Catch
import qualified Control.Exception as Exc
import           Control.Monad.IO.Class
import           Control.Monad.IO.Unlift

-- | Evaluate a 'Catch' effect.
unliftCatch :: (forall x . m x -> IO x)
            -> CatchC m a
            -> m a
unliftCatch handler = runReader (Handler handler) . runCatchC

-- | Evaluate a 'Catch' effect, using 'MonadUnliftIO' to infer a correct
-- unlifting function.
runCatch :: MonadUnliftIO m => CatchC m a -> m a
runCatch c = withRunInIO (\f -> runHandler (Handler f) c)


newtype Handler m = Handler (forall x . m x -> IO x)

runHandler :: Handler m -> CatchC m a -> IO a
runHandler h@(Handler handler) = handler . runReader h . runCatchC


newtype CatchC m a = CatchC { runCatchC :: ReaderC (Handler m) m a }
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadUnliftIO m => MonadUnliftIO (CatchC m) where
  askUnliftIO = CatchC . ReaderC $ \(Handler h) ->
    withUnliftIO $ \u -> pure (UnliftIO $ \r -> unliftIO u (unliftCatch h r))

instance (Carrier sig m, MonadIO m) => Carrier (Catch :+: sig) (CatchC m) where
  eff (L (CatchIO act cleanup k)) = do
    handler <- CatchC ask
    liftIO (Exc.catch (runHandler handler act) (runHandler handler . cleanup)) >>= k
  eff (R other) = CatchC (eff (R (handleCoercible other)))
