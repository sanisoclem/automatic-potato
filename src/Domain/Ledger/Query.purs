module AP.Domain.Ledger.Query where

import Prelude

import Control.Bind (bindFlipped)
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Maybe (Maybe)
import AP.Capability.Fetch (class MonadFetchRequest, class MonadFromRequest, getPath, tryGetParamNumber)
import AP.Domain.Ledger.Identifiers (AccountId, AccountType, TransactionId)
import AP.Data.Instant (Instant, mkInstant)
import AP.Data.Money (Money)
import Effect.Exception (Error, error)


data LedgerQuery
  = GetLedgerV1
  | GetBalancesV1
  | GetTransactionsV1 { from :: Maybe Instant, to :: Maybe Instant }

type GetLedgerResultV1 =
  { name :: String
  , accounts :: Array
    { accountId :: AccountId
    , name :: String
    , accountType :: AccountType
    , denomination :: String
    , closed :: Boolean
    }
  }
type GetBalancesV1 =
  { accountBalances :: Map AccountId { debits :: Money, credits :: Money }
  , floatingBalance :: { debits :: Money, credits :: Money }
  }

data LedgerQueryResult
  = GetLedgerResultV1 GetLedgerResultV1
  | GetBalancesResultV1 GetBalancesV1
  | GetTransactionsResultV1
    ( Array
      { transactionId :: TransactionId
      , date :: Instant
      , credit :: Maybe AccountId
      , debit :: Maybe AccountId
      , amount :: Money
      , notes :: String
      }
    )

derive instance Generic LedgerQueryResult _
instance EncodeJson LedgerQueryResult where
  encodeJson = genericEncodeJson
instance DecodeJson LedgerQueryResult where
  decodeJson = genericDecodeJson

instance (MonadFetchRequest m, MonadThrow Error m) => MonadFromRequest m LedgerQuery where
  fromRequest = do
    path <- getPath
    case path of
      "GetLedgerV1" -> pure GetLedgerV1
      "GetBalancesV1" -> pure GetBalancesV1
      "GetTransactionsV1" -> do
        from <- bindFlipped mkInstant <$> tryGetParamNumber "from"
        to <- bindFlipped mkInstant <$> tryGetParamNumber "to"
        pure $ GetTransactionsV1 { from, to }
      _ -> throwError $ error "Cannot build ledger query from request"