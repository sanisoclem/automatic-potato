module AP.UI.Component.Ledger.Router where

import Prelude

import AP.Capability.ApiClient (class MonadApiClient, Balances, Ledger, refreshLedgerList)
import AP.Domain.Ledger.Identifiers (AccountType(..), ledgerId)
import AP.UI.Capability.Navigate (class MonadNavigate, navigate)
import AP.UI.Component.HTML.Utils (css)
import AP.UI.Component.Ledger.Dashboard (dashboardComponent) as Component.Ledger
import AP.UI.Component.Ledger.Transactions (transactionsComponent) as Component.Ledger
import AP.UI.Component.Utility (OpaqueSlot)
import AP.UI.Part.Button (linkBtn_)
import AP.UI.Route (LedgerRoute)
import AP.UI.Route as Routes
import AP.UI.Store as Store
import Data.Array (filter, find)
import Data.Maybe (Maybe(..), isJust, isNothing)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore)
import Halogen.Store.Select (selectAll)
import Type.Proxy (Proxy(..))

type Input =
  { ledgerId :: String
  , route :: Routes.LedgerRoute
  }

type State =
  { ledgerId :: String
  , ledger:: Maybe Ledger
  , balances :: Maybe Balances
  , activeAccount:: Maybe String
  , route :: LedgerRoute }

data Action
  = Initialize
  | NavigateHome
  | Receive (Connected Store.Store Input)

type ChildSlots =
  ( dashboard :: OpaqueSlot Unit
  , accountLedger :: OpaqueSlot Unit
  , newAccount :: OpaqueSlot Unit
  )

routerComponent
  :: forall q m
   . MonadAff m
  => MonadApiClient m
  => MonadNavigate Routes.Route m
  => MonadStore Store.Action Store.Store m
  => H.Component q Input Void m
routerComponent = connect selectAll $ H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      , receive = Just <<< Receive
      }
  }
  where
  initialState :: Connected Store.Store Input -> State
  initialState { context, input: { ledgerId, route } } =
    { ledgerId
    , route
    , ledger: context.ledgers >>= find (\l -> l.ledgerId == ledgerId)
    , balances: Nothing
    , activeAccount: Nothing -- TODO: get active account from route
    }
  handleAction :: Action -> H.HalogenM State Action ChildSlots Void m Unit
  handleAction = case _ of
    Initialize -> do
      ledger <- H.gets _.ledger
      when (isJust ledger) do
        pure unit -- TODO: get ledger balances
      when (isNothing ledger) do
        refreshLedgerList
    Receive { context, input: { ledgerId, route } } -> do
      oldRoute <- H.gets _.route
      oldLedgerId <- H.gets _.ledgerId
      ledger <- H.gets _.ledger

      when (route /= oldRoute) do
        H.modify_ _
          { route = route
          , activeAccount = Nothing -- TODO: recalc active account
          }
      when (ledgerId /= oldLedgerId || isNothing ledger) do
        H.modify_ _
          { ledgerId = ledgerId
          , ledger = context.ledgers >>= find (\l -> l.ledgerId == ledgerId)
          , balances = Nothing
          }
        -- TODO: get ledger balances
    NavigateHome ->
      navigate $ Routes.Home

  render :: State -> H.ComponentHTML Action ChildSlots m
  render state = case state.ledger of
    Nothing ->  HH.div_ [ HH.text "Ledger not found" ]
    Just { ledgerId, result: ledger } ->
      HH.div
        [ css "flex flex-row "]
        [ HH.div
            [ css "min-h-screen bg-gray-900 w-64 flex flex-col justify-center items-center gap-4"]
            [ HH.h2_
                [ linkBtn_ ledger.name NavigateHome ]
            , HH.div_
                [ HH.div_ [ HH.text "TODO Metrics" ]
                , HH.div_ [ HH.text "Networth: $100,000 +0.8%" ] -- overall net position
                , HH.div_ [ HH.text "Budgeted $ days: 100" ] -- number of dollar-days budgeted, measures predictiveness of budget
                , HH.div_ [ HH.text "Debt ratio: 0.3" ] -- ratio of liabilities to assets, could add a breakdown to separate long term vs short term debt
                , HH.div_ [ HH.text "Historical Volatility:  5%" ] -- how much networth moves
                , HH.div_ [ HH.text "Cashflow: +$10,000" ] -- net cashflow
                ]
             , HH.div_
                [ HH.div_ [ HH.text "TODO: Floating Balance" ]
                ]
            , HH.div_
                [ HH.h3_ [ HH.text "Assets"]
                , HH.ul_ $ filter (\a -> a.accountType == Asset) ledger.accounts <#> \a ->
                    HH.li_ [HH.text a.name ]
                ]
            , HH.div_
                [ HH.h3_ [ HH.text "Liabilities"]
                , HH.ul_ $ filter (\a -> a.accountType == Liability) ledger.accounts <#> \a ->
                    HH.li_ [HH.text a.name ]
                ]
            ]
        , HH.div
            []
            [ case state.route of
              Routes.LedgerDashboard ->
                HH.slot_ (Proxy :: _ "dashboard") unit Component.Ledger.dashboardComponent unit
              Routes.AccountTransactions accountId ->
                HH.slot_ (Proxy :: _ "accountLedger") unit Component.Ledger.transactionsComponent unit
              Routes.AccountEdit accountId ->
                HH.slot_ (Proxy :: _ "accountLedger") unit Component.Ledger.transactionsComponent unit
              Routes.AccountNew ->
                HH.slot_ (Proxy :: _ "accountLedger") unit Component.Ledger.transactionsComponent unit
            ]
        ]


