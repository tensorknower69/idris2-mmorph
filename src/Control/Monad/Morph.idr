module Control.Monad.Morph

import Control.Monad.Either
import Control.Monad.Identity
import Control.Monad.Maybe
import Control.Monad.RWS
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

export
generalize : Applicative m => Identity a -> m a
generalize = pure . runIdentity

public export
interface MFunctor (0 t : (Type -> Type) -> (Type -> Type)) where
  hoist : Monad m => (forall b. m b -> m' b) -> t m a -> t m' a

export
MFunctor (EitherT e) where
  hoist k = MkEitherT . k . runEitherT

export
MFunctor MaybeT where
  hoist k = MkMaybeT . k  . runMaybeT

export
MFunctor (ReaderT s) where
  hoist k m = MkReaderT $ k . flip runReaderT m

export
Monoid s => MFunctor (WriterT s) where
  hoist k m = MkWriterT $ k . unWriterT m

export
MFunctor (StateT s) where
  hoist k m = ST $ k . flip runStateT m

export
MFunctor (RWST r w s) where
  hoist k m = MkRWST $ \r, w, s => k $ unRWST m r w s
