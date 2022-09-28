module AP.UI.Component.Ledger.Router where

import Prelude

import AP.UI.Component.Ledger.Dashboard (dashboardComponent) as Component.Ledger
import AP.UI.Component.Ledger.Transactions (transactionsComponent) as Component.Ledger
import AP.UI.Route as Routes
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Type.Proxy (Proxy(..))

type Input = Routes.LedgerRoute

type OpaqueSlot slot = forall query. H.Slot query Void slot

type State = Routes.LedgerRoute

data Action
  = Receive Input

type ChildSlots =
  ( dashboard :: forall query. H.Slot query Void Unit
  , accountLedger :: forall query. H.Slot query Void Unit
  )

routerComponent
  :: forall q m
   . MonadAff m
  => H.Component q Input Void m
routerComponent = H.mkComponent
  { initialState: identity
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , receive = Just <<< Receive
      }
  }
  where
  handleAction :: Action -> H.HalogenM State Action ChildSlots Void m Unit
  handleAction = case _ of
    Receive x -> do
      H.put x

  render :: State -> H.ComponentHTML Action ChildSlots m
  render = case _ of
    Routes.LedgerDashboard ->
      HH.slot_ (Proxy :: _ "dashboard") unit Component.Ledger.dashboardComponent unit
    Routes.AccountLedger accountId ->
      HH.slot_ (Proxy :: _ "accountLedger") unit Component.Ledger.transactionsComponent unit

