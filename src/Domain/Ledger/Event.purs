module AP.Domain.Ledger.Event where

import Prelude

import AP.Capability.Outbox (class OutboxEvent)
import AP.Data.Utility (convertJsonErrorToError)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Generic.Rep (class Generic)

data LedgerEvent
  = LedgerUpdated
  | AccountCreated
  | AccountUpdated
  | AccountClosed
  | BalanceUpdated
  | TransactionCreated
  | TransactionUpdated
  | TransactionDeleted

derive instance Generic LedgerEvent _
instance OutboxEvent LedgerEvent where
  encodeEvent = genericEncodeJson
  decodeEvent = convertJsonErrorToError <<< genericDecodeJson