module AP.Capability.Now where

import Prelude

import AP.Data.Instant (Instant)
import Data.DateTime (Date, DateTime, Time)

class Monad m <= MonadNow m where
  now :: m Instant
  nowDate :: m Date
  nowTime :: m Time
  nowDateTime :: m DateTime
