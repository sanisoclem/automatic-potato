module AP.UI.Route where

import Prelude hiding ((/))

import AP.Domain.Ledger.Identifiers (AccountId(..))
import Data.Generic.Rep (class Generic)
import Data.Lens.Iso.Newtype (_Newtype)
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
  | AccountNew
  | AccountTransactions AccountId
  | AccountEdit AccountId

derive instance Generic LedgerRoute _
derive instance Eq LedgerRoute
derive instance Ord LedgerRoute

accountIdCodec :: RouteDuplex' AccountId
accountIdCodec = _Newtype (string segment)

ledgerRouteCodec :: RouteDuplex' LedgerRoute
ledgerRouteCodec = sum
  { "LedgerDashboard": noArgs
  , "AccountNew" : "new-acct" / noArgs
  , "AccountTransactions": "acct" / accountIdCodec / "txn"
  , "AccountEdit" : "acct" / accountIdCodec / "edit"
  }

routeCodec :: RouteDuplex' Route
routeCodec = root $ sum
  { "Home": noArgs
  , "Ledger": "ledger" / string segment / ledgerRouteCodec
  }

toHref :: Route -> String
toHref = print routeCodec