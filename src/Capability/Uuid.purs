module AP.Capability.Uuid where

import Prelude

import Data.UUID (genUUID, toString)
import Effect.Class (class MonadEffect, liftEffect)

class Monad m <= MonadUuid m where
  generate :: m String

instance MonadEffect m => MonadUuid m where
  generate = toString <$> liftEffect genUUID
