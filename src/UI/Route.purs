module AP.UI.Route where

import Prelude hiding ((/))

import Data.Generic.Rep (class Generic)
import Routing.Duplex (RouteDuplex', print, root, segment, string)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/))

data Route
  = Home
  | Ledger String LedgerRoute

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
  , "Ledger": "ledger" / string segment / ledgerRouteCodec
  }

toHref :: Route -> String
toHref = print routeCodec