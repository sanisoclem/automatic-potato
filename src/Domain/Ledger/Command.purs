module AP.Domain.Ledger.Command where

import Control.Monad.Error.Class (class MonadThrow)
import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import AP.Capability.Fetch (class MonadFetchRequest, class MonadFromRequest, getBodyJson)
import AP.Domain.Ledger.Identifiers (AccountId, AccountType, TransactionId)
import AP.Data.Instant (Instant)
import AP.Data.Money (Money)
import Effect.Exception (Error)

data LedgerCommand
  = UpdateLedgerV1
      { name :: String
      }
  | CreateAccountV1
      { name :: String
      , accountType :: AccountType
      , denomination :: String
      }
  | UpdateAccountV1
      { accountId :: AccountId
      , name :: String
      }
  | CloseAccountV1 AccountId
  | CreateTransactionV1
      { date :: Instant
      , credit :: Maybe AccountId
      , debit :: Maybe AccountId
      , amount :: Money
      , notes :: String
      }
  | UpdateTransactionV1
      { date :: Instant
      , transactionId :: TransactionId
      , credit :: Maybe AccountId
      , debit :: Maybe AccountId
      , amount :: Money
      , notes :: String
      }
  | DeleteTransactionV1 TransactionId

derive instance Generic LedgerCommand _
instance EncodeJson LedgerCommand where
  encodeJson = genericEncodeJson
instance DecodeJson LedgerCommand where
  decodeJson = genericDecodeJson

instance (MonadFetchRequest m, MonadThrow Error m) => MonadFromRequest m LedgerCommand where
  fromRequest = getBodyJson