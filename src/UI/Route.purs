module AP.UI.Route where

import Prelude hiding ((/))

import Data.Generic.Rep (class Generic)
import Routing.Duplex (RouteDuplex', root, segment, string)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/))

data Route
  = Home
  | Ledger LedgerRoute

derive instance Generic Route _
derive instance Eq Route
derive instance Ord Route

data LedgerRoute
  = LedgerDashboard
  | AccountLedger String

derive instance Generic LedgerRoute _
derive instance Eq LedgerRoute
derive instance Ord LedgerRoute

ledgerRouteCodec :: RouteDuplex' LedgerRoute
ledgerRouteCodec = sum
  { "LedgerDashboard": noArgs
  , "AccountLedger": "account" / string segment
  }

routeCodec :: RouteDuplex' Route
routeCodec = root $ sum
  { "Home": noArgs
  , "Ledger": "ledger" / ledgerRouteCodec
  }

data Test
  = Test1
  | Test2 String

derive instance Generic Test _

testCodec :: RouteDuplex' Test
testCodec = root $ sum
  { "Test1": noArgs
  , "Test2": "tae" / string segment / "edit"
  }