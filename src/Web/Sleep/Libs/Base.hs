{-# LANGUAGE DefaultSignatures #-}



module Web.Sleep.Libs.Base where

import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Cont
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Identity
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Resource
import qualified Control.Monad.Trans.RWS.Lazy      as L
import qualified Control.Monad.Trans.RWS.Strict    as S
import qualified Control.Monad.Trans.State.Lazy    as L
import qualified Control.Monad.Trans.State.Strict  as S
import qualified Control.Monad.Trans.Writer.Lazy   as L
import qualified Control.Monad.Trans.Writer.Strict as S
import           Data.Functor.Identity


class Monad m => MonadBase m where
  type Base m :: * -> *
  liftBase :: Base m a -> m a
  default liftBase :: (MonadTrans t, MonadBase b, m ~ t b, Base m ~ Base b) => Base m a -> m a
  liftBase = liftDefault


instance MonadBase IO         where { type Base IO         = IO      ; liftBase = id }
instance MonadBase Maybe      where { type Base Maybe      = Maybe   ; liftBase = id }
instance MonadBase (Either e) where { type Base (Either e) = Either e; liftBase = id }
instance MonadBase ((->) r)   where { type Base ((->) r)   = ((->) r); liftBase = id }
instance MonadBase Identity   where { type Base Identity   = Identity; liftBase = id }

instance MonadBase m => MonadBase (IdentityT  m)  where type Base (IdentityT  m) = Base m
instance MonadBase m => MonadBase (MaybeT     m)  where type Base (MaybeT     m) = Base m
instance MonadBase m => MonadBase (ReaderT  r m)  where type Base (ReaderT  r m) = Base m
instance MonadBase m => MonadBase (L.StateT s m)  where type Base (L.StateT s m) = Base m
instance MonadBase m => MonadBase (S.StateT s m)  where type Base (S.StateT s m) = Base m
instance MonadBase m => MonadBase (ContT    r m)  where type Base (ContT    r m) = Base m
instance MonadBase m => MonadBase (ExceptT  e m)  where type Base (ExceptT  e m) = Base m
instance MonadBase m => MonadBase (ResourceT  m)  where type Base (ResourceT  m) = Base m

instance (Monoid w, MonadBase m) => MonadBase (L.WriterT w     m) where type Base (L.WriterT w     m) = Base m
instance (Monoid w, MonadBase m) => MonadBase (S.WriterT w     m) where type Base (S.WriterT w     m) = Base m
instance (Monoid w, MonadBase m) => MonadBase (L.RWST    r w s m) where type Base (L.RWST    r w s m) = Base m
instance (Monoid w, MonadBase m) => MonadBase (S.RWST    r w s m) where type Base (S.RWST    r w s m) = Base m


liftDefault :: (MonadTrans t, MonadBase b, m ~ t b, Base m ~ Base b) => Base m a -> m a
liftDefault = lift . liftBase
