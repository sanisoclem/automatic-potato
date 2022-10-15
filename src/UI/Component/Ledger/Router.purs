module AP.UI.Component.Ledger.Router where

import Prelude

import AP.Data.Money (Money(..), toString)
import AP.Domain.Ledger.Identifiers (AccountId(..), AccountType(..), accountId, ledgerId)
import AP.UI.Capability.ApiClient (class MonadApiClient, Balances, Ledger, getBalances, refreshLedgerList)
import AP.UI.Capability.Navigate (class MonadNavigate, navigate)
import AP.UI.Component.HTML.Utils (css)
import AP.UI.Component.Ledger.Dashboard (dashboardComponent) as Component.Ledger
import AP.UI.Component.Ledger.EditAccount (editAccountComponent) as Component.Ledger
import AP.UI.Component.Ledger.Transactions (transactionsComponent) as Component.Ledger
import AP.UI.Component.Utility (OpaqueSlot)
import AP.UI.Part.Button (linkBtn_)
import AP.UI.Route (LedgerRoute)
import AP.UI.Route as Routes
import AP.UI.Store as Store
import Data.Array (filter, find)
import Data.Map (lookup)
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing)
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
    , ledger: context.ledgers >>= find (\l -> l.ledgerId == ledgerId)
    , balances: Nothing
    , activeAccount: Nothing -- TODO: get active account from route
    }
  handleAction :: Action -> H.HalogenM State Action ChildSlots Void m Unit
  handleAction = case _ of
    Initialize -> do
      ledger <- H.gets _.ledger
      when (isNothing ledger) do
        refreshLedgerList
      balances <- H.gets _.ledgerId >>= getBalances
      H.modify_ _ { balances = Just balances }
    Receive { context, input: { ledgerId, route } } -> do
      oldRoute <- H.gets _.route
      oldLedgerId <- H.gets _.ledgerId
      ledger <- H.gets _.ledger

      when (route /= oldRoute) do
        H.modify_ _
          { route = route
          , activeAccount = Nothing -- TODO: recalc active account
          }

      H.modify_ _
        { ledgerId = ledgerId
        , ledger = context.ledgers >>= find (\l -> l.ledgerId == ledgerId)
        }
      when (oldLedgerId /= ledgerId) do
        H.modify_ _
          { balances = Nothing
          }
        balances <- getBalances ledgerId
        H.modify_ _ { balances = Just balances }

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
            [ css "min-h-screen bg-gray-900 w-64 flex flex-col justify-center items-center gap-4 text-center"]
            [ HH.h2
                [ css "text-xl"]
                [ linkBtn_ ledger.result.name NavigateHome ]
            , HH.div
                [ css "text-xs"]
                [ HH.div_ [ HH.text "TODO Metrics" ]
                -- , HH.div_ [ HH.text "Networth: $100,000 +0.8%" ] -- overall net position
                -- , HH.div_ [ HH.text "Budgeted $ days: 100" ] -- number of dollar-days budgeted, measures predictiveness of budget
                -- , HH.div_ [ HH.text "Debt ratio: 0.3" ] -- ratio of liabilities to assets, could add a breakdown to separate long term vs short term debt
                -- , HH.div_ [ HH.text "Historical Volatility:  5%" ] -- how much networth moves
                -- , HH.div_ [ HH.text "Cashflow: +$10,000" ] -- net cashflow
                ]
             , HH.div
                [ css "text-xs"]
                [ HH.div_ [ HH.text "TODO: Floating Balance" ]
                ]
            , HH.div_
                [ HH.h3_ [ HH.text "Assets"]
                , HH.ul_ $ filter (\a -> a.accountType == Asset) ledger.result.accounts <#> \a ->
                    HH.li_
                      [ linkBtn_ a.name $ NavigateAccountTransactions a.accountId
                      , HH.span_ [ HH.text $ getBalance state a.accountId ]
                      ]
                ]
            , HH.div_
                [ HH.h3_ [ HH.text "Liabilities"]
                , HH.ul_ $ filter (\a -> a.accountType == Liability) ledger.result.accounts <#> \a ->
                    HH.li_
                      [ linkBtn_ a.name $ NavigateAccountTransactions a.accountId
                      , HH.span_ [ HH.text $ getBalance state a.accountId ]
                      ]
                ]
            , HH.div_
                [ HH.h3_ [ HH.text "Actions"]
                , HH.ul_
                    [ HH.li_ [ linkBtn_ "New Account" NavigateAccountNew ]
                    ]
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
                HH.slot_ (Proxy :: _ "editAccount") unit Component.Ledger.editAccountComponent ({ ledger, accountId: Just accountId })
              Routes.AccountNew ->
                HH.slot_ (Proxy :: _ "editAccount") unit Component.Ledger.editAccountComponent ({ ledger, accountId: Nothing })
            ]
        ]


