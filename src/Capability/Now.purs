module AP.Capability.Now where

import Prelude

import AP.Data.Instant (Instant, fromStdInstant)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Data.DateTime (Date, DateTime, Time)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Now as Now

class Monad m <= MonadNow m where
  now :: m Instant
  nowDate :: m Date
  nowTime :: m Time
  nowDateTime :: m DateTime

instance (MonadTrans t, MonadNow m, Monad (t m)) => MonadNow (t m) where
  now = lift now
  nowDate = lift nowDate
  nowTime = lift nowTime
  nowDateTime = lift nowDateTime
else instance (MonadEffect m) => MonadNow m where
  now = fromStdInstant <$> liftEffect Now.now
  nowDate = liftEffect Now.nowDate
  nowTime = liftEffect Now.nowTime
  nowDateTime = liftEffect Now.nowDateTime