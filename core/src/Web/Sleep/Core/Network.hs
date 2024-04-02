module Web.Sleep.Core.Network where

import "this" Prelude

import Data.ByteString.Lazy   (toStrict)
import Network.HTTP.Client    qualified as N

import Web.Sleep.Core.Command
import Web.Sleep.Core.Error


--------------------------------------------------------------------------------
-- Network class

class (Monad m) => MonadNetwork m where
  askManager :: m N.Manager

instance (MonadNetwork m) => MonadNetwork (ExceptT e m) where
  askManager = lift askManager

instance (MonadNetwork m) => MonadNetwork (ReaderT r m) where
  askManager = lift askManager

instance (MonadNetwork m) => MonadNetwork (StateT s m) where
  askManager = lift askManager

instance (Monoid w, MonadNetwork m) => MonadNetwork (WriterT w m) where
  askManager = lift askManager


--------------------------------------------------------------------------------
-- Running commands

{-| Runs a command and parses the output.

This function is provided as part of the "simple" API: as a user, you might
prefer to reimplement it to have full control over how the network calls are
performed, how errors are handled, on what errors the request should be retried,
and so on.

Any non-2xx resonse with result in an IO HttpException being thrown. Any error
in the payload will result in an 'Error' being thrown in the calling
'MonadError' monad.
-}
runCommand
  :: (MonadIO m, MonadNetwork m, MonadError Error m)
  => Command a
  -> m a
runCommand cmd = do
  manager <- askManager
  resultBS <- liftIO $ N.httpLbs (N.setRequestCheckStatus $ request cmd) manager
  liftEither $ decode cmd $ toStrict $ N.responseBody resultBS
