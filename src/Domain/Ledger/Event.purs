module AP.Domain.Ledger.Event where

import Prelude

import AP.Capability.Outbox (class OutboxEvent)
import Data.Argonaut (printJsonDecodeError)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Bifunctor (lmap)
import Data.Generic.Rep (class Generic)
import Effect.Exception (error)

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
  decodeEvent = lmap (error <<< printJsonDecodeError) <<< genericDecodeJson