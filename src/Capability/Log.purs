module AP.Capability.Log where

import Prelude

import Control.Monad.Trans.Class (lift)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import AP.Capability.Now (class MonadNow, nowDateTime)
import AP.Data.Log (Log, LogLevel(..), mkLog)

class Monad m <= MonadLog m where
  logMessage :: Log -> m Unit

log :: forall m. MonadLog m => MonadNow m => LogLevel -> String -> m Unit
log level msg = do
  now <- nowDateTime
  logMessage <<< mkLog now level $ msg

-- | Log a message for debugging purposes
logDebug :: forall m. MonadLog m => MonadNow m => String -> m Unit
logDebug = log Debug

-- | Log a message to convey non-error information
logInfo :: forall m. MonadLog m => MonadNow m => String -> m Unit
logInfo = log Info

-- | Log a message as a warning
logWarn :: forall m. MonadLog m => MonadNow m => String -> m Unit
logWarn = log Warn

-- | Log a message as an error
logError :: forall m. MonadLog m => MonadNow m => String -> m Unit
logError = log Error

-- | Hush a monadic action by logging the error, leaving it open why the error is being logged
logHush :: forall m a. MonadLog m => MonadNow m => LogLevel -> m (Either String a) -> m (Maybe a)
logHush level action =
  action >>= case _ of
    Left e -> case level of
      Debug -> logDebug e *> pure Nothing
      Info -> logInfo e *> pure Nothing
      Warn -> logWarn e *> pure Nothing
      Error -> logError e *> pure Nothing
    Right v -> pure $ Just v

-- | Hush a monadic action by logging the error in debug mode
debugHush :: forall m a. MonadLog m => MonadNow m => m (Either String a) -> m (Maybe a)
debugHush = logHush Debug
