module AP.Capability.ApiClient where

import Prelude
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Data.Maybe (Maybe)

type Session =
  { sub :: Int
  , allow :: Boolean
  , name :: String
  , exp :: Int
  }

type Ledger =
  { ledgerId :: String
  , name :: String }

class Monad m <= MonadApiClient m where
  getSession :: m (Maybe Session)
  getLedgers :: m (Array Ledger)
  createLedger :: String -> m Unit

instance (MonadTrans t, MonadApiClient m, Monad (t m)) => MonadApiClient (t m) where
  getSession = lift getSession
  getLedgers = lift getLedgers
  createLedger = lift <<< createLedger