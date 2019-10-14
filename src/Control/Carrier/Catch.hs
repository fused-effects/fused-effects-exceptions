{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, RankNTypes, TypeOperators, UndecidableInstances #-}

-- | A carrier for a 'Catch' effect.
module Control.Carrier.Catch
  ( -- * Catch effect
    module Control.Effect.Catch
    -- * Catch carrier
  , runCatch
  , CatchC (..)
    -- * Re-exports
  , Carrier
  , run
  ) where

import           Control.Applicative (Alternative(..))
import           Control.Carrier
import           Control.Effect.Catch
import qualified Control.Exception as Exc
import           Control.Monad (MonadPlus(..))
import qualified Control.Monad.Fail as Fail
import           Control.Monad.Fix
import           Control.Monad.IO.Class
import           Control.Monad.IO.Unlift
import           Control.Monad.Trans.Class

-- | Evaluate a 'Catch' effect, using 'MonadUnliftIO' to infer a correct unlifting function.
--
-- | @since 1.0.0.0
runCatch :: MonadUnliftIO m => CatchC m a -> m a
runCatch = runCatchC

newtype CatchC m a = CatchC { runCatchC :: m a }
  deriving (Alternative, Applicative, Functor, Monad, Fail.MonadFail, MonadFix, MonadIO, MonadPlus)

instance MonadTrans CatchC where
  lift = CatchC

instance MonadUnliftIO m => MonadUnliftIO (CatchC m) where
  withRunInIO f = CatchC (withRunInIO (\ runInIO -> f (runInIO . runCatchC)))

instance (Carrier sig m, MonadUnliftIO m) => Carrier (Catch :+: sig) (CatchC m) where
  eff (L (CatchIO act cleanup k)) = do
    handler <- askUnliftIO
    liftIO (Exc.catch (unliftIO handler act) (unliftIO handler . cleanup)) >>= k
  eff (R other) = CatchC (eff (handleCoercible other))
