module AP.UI.Capability.ApiClient where

import Prelude

import AP.Data.Instant (Instant(..))
import AP.Data.Money (Money(..))
import AP.Domain.Ledger.Identifiers (AccountId(..), AccountType, TransactionId(..))
import AP.Domain.Ledger.Query (GetLedgerResultV1, GetBalancesV1)
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
type Balances = GetBalancesV1
type CreateLedger =
  { ledgerId :: String
  , name :: String
  , accountType:: AccountType
  , denomination :: String }
type TxnFields =
  ( date :: Instant
  , credit :: Maybe AccountId
  , debit :: Maybe AccountId
  , amount :: Money
  , notes :: String
  )
type Account =
  { accountId :: AccountId
  , name :: String
  , accountType :: AccountType
  , denomination :: String
  , closed :: Boolean
  }
type NewTransaction = { | TxnFields }
type Transaction = { transactionId :: TransactionId | TxnFields }

class Monad m <= MonadApiClient m where
  getSession :: m (Maybe Session)
  refreshLedgerList :: m Unit
  createLedger :: String -> m Unit
  createAccount :: CreateLedger -> m Unit
  updateBalances :: String -> m Unit

instance (MonadTrans t, MonadApiClient m, Monad (t m)) => MonadApiClient (t m) where
  getSession = lift getSession
  createLedger = lift <<< createLedger
  refreshLedgerList = lift refreshLedgerList
  createAccount = lift <<< createAccount
  updateBalances = lift <<< updateBalances