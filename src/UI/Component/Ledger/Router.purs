module AP.UI.Component.Ledger.Router where

import Prelude

import AP.Data.Money (toString)
import AP.Domain.Ledger.Identifiers (AccountId, AccountType(..))
import AP.UI.Capability.ApiClient (class MonadApiClient, Balances, Ledger, Account, refreshLedgerList, updateBalances)
import AP.UI.Capability.Navigate (class MonadNavigate, navigate)
import AP.UI.Component.HTML.Utils (css)
import AP.UI.Component.Ledger.Dashboard (dashboardComponent) as Component.Ledger
import AP.UI.Component.Ledger.EditAccount (editAccountComponent) as Component.Ledger
import AP.UI.Component.Ledger.Transactions (transactionsComponent) as Component.Ledger
import AP.UI.Component.Utility (OpaqueSlot)
import AP.UI.Part.Button (linkAction)
import AP.UI.Route (LedgerRoute)
import AP.UI.Route as Routes
import AP.UI.Store as Store
import Data.Array (filter, find, singleton)
import Data.Map (lookup)
import Data.Maybe (Maybe(..), fromMaybe, isNothing)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
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
  , activeAccount:: Maybe Account
  , route :: LedgerRoute }

data Action
  = Initialize
  | Receive (Connected Store.Store Input)
  | NavigateHome
  | NavigateAccountNew
  | NavigateAccountEdit AccountId
  | NavigateAccountTransactions AccountId

type ChildSlots =
  ( dashboard :: OpaqueSlot Unit
  , accountLedger :: OpaqueSlot Unit
  , editAccount :: OpaqueSlot Unit
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
    , ledger: activeLedger context ledgerId
    , balances: lookup ledgerId context.balances
    , activeAccount: activeAccount context ledgerId route
    }
  activeLedger context ledgerId = context.ledgers >>= find (\l -> l.ledgerId == ledgerId)
  activeAccount context ledgerId route = case route of
    Routes.AccountTransactions id ->
      activeLedger context ledgerId <#> _.result.accounts >>= find (\a -> a.accountId == id)
    _ -> Nothing
  handleAction :: Action -> H.HalogenM State Action ChildSlots Void m Unit
  handleAction = case _ of
    Initialize -> do
      ledger <- H.gets _.ledger
      when (isNothing ledger) do
        refreshLedgerList

      ledgerId <- H.gets _.ledgerId
      balances <- H.gets  _.balances
      when (isNothing balances) do
        updateBalances ledgerId
    Receive { context, input: { ledgerId, route } } -> do
      H.modify_ _
        { ledgerId = ledgerId
        , ledger = activeLedger context ledgerId
        , balances = lookup ledgerId context.balances
        , route = route
        , activeAccount = activeAccount context ledgerId route -- TODO: recalc active account
        }

    NavigateHome ->
      navigate $ Routes.Home
    NavigateAccountNew -> do
      ledgerId <- H.gets _.ledgerId
      navigate $ Routes.Ledger ledgerId Routes.AccountNew
    NavigateAccountEdit accountId -> do
      ledgerId <- H.gets _.ledgerId
      navigate $ Routes.Ledger ledgerId $ Routes.AccountEdit accountId
    NavigateAccountTransactions accountId -> do
      ledgerId <- H.gets _.ledgerId
      navigate $ Routes.Ledger ledgerId $ Routes.AccountTransactions accountId
  getBalance :: State -> AccountId -> String
  getBalance state accountId =
    state.balances
    <#> _.accountBalances
    >>= lookup accountId
    <#> (\{credits, debits} -> debits - credits )
    # fromMaybe zero
    # toString
  render :: State -> H.ComponentHTML Action ChildSlots m
  render state = case state.ledger of
    Nothing ->  HH.div_ [ HH.text "Ledger not found" ]
    Just ledger ->
      HH.div
        [ css "flex flex-row "]
        [ HH.div
            [ css "min-h-screen bg-gray-800 flex-grow-0 flex-shrink-0 p-4 w-3/12 flex flex-col gap-4"]
            [ HH.div_
                [ HH.h2
                    [ css "text-xl"]
                    [ HH.text ledger.result.name ]
                , HH.div_ <<< singleton $ linkAction mempty [ HE.onClick \_ -> NavigateHome ] [ HH.sub_ [ HH.text "Open Budget" ] ]
                , HH.div_ <<< singleton $ linkAction mempty [ HE.onClick \_ -> NavigateAccountNew ] [ HH.sub_ [ HH.text "New Account" ] ]
                ]
            -- , HH.div
            --     [ css "text-xs"]
            --     [ HH.div_ [ HH.text "TODO Metrics" ]
                -- , HH.div_ [ HH.text "Networth: $100,000 +0.8%" ] -- overall net position
                -- , HH.div_ [ HH.text "Budgeted $ days: 100" ] -- number of dollar-days budgeted, measures predictiveness of budget
                -- , HH.div_ [ HH.text "Debt ratio: 0.3" ] -- ratio of liabilities to assets, could add a breakdown to separate long term vs short term debt
                -- , HH.div_ [ HH.text "Historical Volatility:  5%" ] -- how much networth moves
                -- , HH.div_ [ HH.text "Cashflow: +$10,000" ] -- net cashflow
            --     ]
            --  , HH.div
            --     [ css "text-xs"]
            --     [ HH.div_ [ HH.text "TODO: Floating Balance" ]
            --     ]
            , HH.div_
                [ HH.h3_ [ HH.text "Assets"]
                , HH.ul [ css "pl-4" ] $ filter (\a -> a.accountType == Asset) ledger.result.accounts <#> \a ->
                    HH.li_
                      [ linkAction " flex flex-row justify-between"
                          [ HE.onClick \_ -> NavigateAccountTransactions a.accountId ]
                          [ HH.text a.name
                          , HH.span_ [ HH.text $ (getBalance state a.accountId) <> " " <> a.denomination ]
                          ]
                      ]
                ]
            , HH.div_
                [ HH.h3_ [ HH.text "Liabilities"]
                , HH.ul [ css "pl-4" ] $ filter (\a -> a.accountType == Liability) ledger.result.accounts <#> \a ->
                    HH.li_
                      [ linkAction " flex flex-row justify-between"
                          [ HE.onClick \_ -> NavigateAccountTransactions a.accountId ]
                          [ HH.text a.name
                          , HH.span_ [ HH.text $ (getBalance state a.accountId) <> " " <> a.denomination ]
                          ]
                      ]
                ]
            ]
        , HH.div
            [ css "flex-grow justify-self-stretch relative p-4" ]
            [ case { route: state.route, account: state.activeAccount} of
              { route: Routes.LedgerDashboard } ->
                HH.slot_ (Proxy :: _ "dashboard") unit Component.Ledger.dashboardComponent unit
              { route: Routes.AccountTransactions accountId, account: Just account } ->
                HH.slot_ (Proxy :: _ "accountLedger") unit Component.Ledger.transactionsComponent { ledger, account }
              { route: Routes.AccountEdit accountId } ->
                HH.slot_ (Proxy :: _ "editAccount") unit Component.Ledger.editAccountComponent ({ ledger, accountId: Just accountId })
              { route: Routes.AccountNew } ->
                HH.slot_ (Proxy :: _ "editAccount") unit Component.Ledger.editAccountComponent ({ ledger, accountId: Nothing })
              _ -> HH.text "account not found"
            ]
        ]


