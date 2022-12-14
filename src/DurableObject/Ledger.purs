module AP.DurableObject.Ledger where

import Prelude

import AP.Capability.Now (class MonadNow, now)
import AP.Capability.Outbox (class MonadOutbox, queue)
import AP.Capability.Uuid (generate)
import AP.Domain.Ledger.Command (LedgerCommand(..))
import AP.Domain.Ledger.Event (LedgerEvent(..))
import AP.Domain.Ledger.Identifiers (accountId, transactionId)
import AP.Domain.Ledger.LedgerDb (class MonadLedgerDb, class MonadLedgerReadonlyDb, deleteTransaction, ensureAccountExists, getAccount, getAccountsReadonly, getBalancesReadonly, getLedger, getLedgerReadonly, getTransaction, getTransactionsReadonly, postTransaction, putAccount, putLedger, putTransaction, updateBalances)
import AP.Domain.Ledger.Query (LedgerQuery(..))
import Control.Monad.Error.Class (class MonadThrow)
import Data.Argonaut (Json, encodeJson)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (sequence)
import Effect.Class (class MonadEffect)
import Effect.Exception (Error)

handleCommand
  :: ∀ m
   . MonadLedgerDb m
  => MonadOutbox LedgerEvent m
  => MonadThrow Error m
  => MonadEffect m
  => MonadNow m
  => LedgerCommand
  -> m Unit
handleCommand = case _ of
    UpdateLedgerV1 x -> do
      ledger <- getLedger
      createdAt <- now
      putLedger $
        case ledger of
          Just l -> l { name = x.name }
          _ -> { name: x.name, createdAt }
      queue LedgerUpdated
    CreateAccountV1 x -> do
      accountId <- accountId <$> generate
      putAccount
        { accountId
        , accountType: x.accountType
        , denomination: x.denomination
        , name: x.name
        , closed: false
        }
      queue AccountCreated
    UpdateAccountV1 x -> do
      account <- getAccount x.accountId
      putAccount account { name = x.name }
      queue AccountUpdated
    CloseAccountV1 accountId -> do
      account <- getAccount accountId
      -- ensure "Account balance should be zero" $ account.balance == zero
      putAccount account { closed = true }
      queue AccountClosed
    CreateTransactionV1 x -> do
      transactionId <- transactionId <$> generate
      void <<< sequence $ ensureAccountExists <$> x.credit
      void <<< sequence $ ensureAccountExists <$> x.debit
      postTransaction
        { transactionId
        , date: x.date
        , credit: x.credit
        , debit: x.debit
        , amount: x.amount
        , notes: x.notes
        }
      updateBalances x.amount x.credit x.debit
      queue TransactionCreated
      queue BalanceUpdated
    UpdateTransactionV1 x -> do
      prevTrans <- getTransaction x.transactionId
      void <<< sequence $ ensureAccountExists <$> x.credit
      void <<< sequence $ ensureAccountExists <$> x.debit

      -- reverse prev transaction
      updateBalances (-prevTrans.amount) prevTrans.credit prevTrans.debit

      let updatedTrans = prevTrans { amount = x.amount
        , notes = x.notes
        , date = x.date
        , debit = x.debit
        , credit = x.credit
      }

      putTransaction updatedTrans
      updateBalances updatedTrans.amount updatedTrans.credit updatedTrans.debit
      queue TransactionUpdated
      queue BalanceUpdated
    DeleteTransactionV1 transactionId -> do
      trans <- getTransaction transactionId
      updateBalances trans.amount trans.credit trans.debit
      deleteTransaction transactionId
      queue TransactionDeleted
      queue BalanceUpdated

handleQuery
  :: ∀ m
   . MonadLedgerReadonlyDb m
  => MonadThrow Error m
  => LedgerQuery
  -> m Json
handleQuery = case _ of
  GetLedgerV1 -> do
    ledger <- getLedgerReadonly
    accounts <- getAccountsReadonly
    pure $
      encodeJson
        { name: fromMaybe "" (ledger <#> _.name)
        , accounts: accounts
        }
  GetBalancesV1 -> do
    encodeJson <$> getBalancesReadonly
  GetTransactionsV1 x -> do
    encodeJson <$> getTransactionsReadonly x.from x.to