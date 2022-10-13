module AP.Capability.ApiClient where

import Prelude

import AP.Domain.Ledger.Query (GetLedgerResultV1)
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
  , result :: GetLedgerResultV1 }

class Monad m <= MonadApiClient m where
  getSession :: m (Maybe Session)
  getLedger :: String -> m Ledger
  getLedgers :: m (Array Ledger)
  createLedger :: String -> m Unit

instance (MonadTrans t, MonadApiClient m, Monad (t m)) => MonadApiClient (t m) where
  getSession = lift getSession
  getLedger = lift <<< getLedger
  getLedgers = lift getLedgers
  createLedger = lift <<< createLedger