module AP.Domain.Ledger.LedgerDb where

import Prelude

import AP.Data.Instant (Instant)
import AP.Data.Money (Money, zeroMoney)
import AP.Domain.Ledger.Database (AccountDocumentRecord, LedgerBalanceDocumentRecord, LedgerDocumentRecord, TransactionDocumentRecord)
import AP.Domain.Ledger.Identifiers (AccountId, TransactionId)
import Data.Map (alter)
import Data.Maybe (Maybe(..), fromMaybe)

class Monad m <= MonadLedgerReadonlyDb m where
  getLedgerReadonly :: m (Maybe LedgerDocumentRecord)
  getAccountReadonly :: AccountId -> m AccountDocumentRecord
  getAccountsReadonly :: m (Array AccountDocumentRecord)
  getBalancesReadonly :: m LedgerBalanceDocumentRecord
  getTransactionReadonly :: TransactionId -> m TransactionDocumentRecord
  getTransactionsReadonly :: Maybe Instant -> Maybe Instant -> m (Array TransactionDocumentRecord)

class Monad m <= MonadLedgerDb m where
  getLedger :: m (Maybe LedgerDocumentRecord)
  putLedger :: LedgerDocumentRecord -> m Unit
  getAccount :: AccountId -> m AccountDocumentRecord
  ensureAccountExists :: AccountId -> m Unit
  putAccount :: AccountDocumentRecord -> m Unit
  getBalances :: m LedgerBalanceDocumentRecord
  putBalances :: LedgerBalanceDocumentRecord -> m Unit
  getTransaction :: TransactionId -> m TransactionDocumentRecord
  putTransaction :: TransactionDocumentRecord -> m Unit
  postTransaction :: TransactionDocumentRecord -> m Unit
  deleteTransaction :: TransactionId -> m Unit

type CreditDebitOperation = { accountId:: AccountId, amount :: Money }
mkCdo ∷ Money -> AccountId -> CreditDebitOperation
mkCdo amount accountId = { accountId, amount }

updateBalances :: forall m. MonadLedgerDb m => Money -> Maybe AccountId -> Maybe AccountId -> m Unit
updateBalances amount creditAct debitAct = do
  updatedBalance <- debit amount debitAct <<< credit amount creditAct <$> getBalances
  putBalances updatedBalance
  where
    credit amt ca bal = case ca of
      Just act -> bal { accountBalances = alter (Just <<< fromMaybe { debits: zeroMoney, credits: amt } <<< map (\b -> b { credits = b.credits + amt })) act bal.accountBalances }
      Nothing -> bal { floatingBalance = bal.floatingBalance { credits = bal.floatingBalance.credits + amt } }
    debit amt da bal = case da of
      Just act -> bal { accountBalances = alter (Just <<< fromMaybe { credits: zeroMoney, debits: amt } <<< map (\b -> b { debits = b.debits + amt })) act bal.accountBalances }
      Nothing -> bal { floatingBalance = bal.floatingBalance { debits = bal.floatingBalance.debits + amt } }

