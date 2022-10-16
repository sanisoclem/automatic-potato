module AP.UI.Component.Home
  ( homeComponent
  )
  where

import Prelude

import AP.UI.Capability.ApiClient (class MonadApiClient, Ledger, Session, createLedger, refreshLedgerList)
import AP.Capability.Log (class MonadLog, logDebug)
import AP.Capability.Now (class MonadNow)
import AP.UI.Capability.Navigate (class MonadNavigate, navigate)
import AP.UI.Component.HTML.Utils (css)
import AP.UI.Component.Ledger.CreateLedger as CreateLedger
import AP.UI.Part.Button (btn_, link_)
import AP.UI.Route as Routes
import AP.UI.Store as Store
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore)
import Halogen.Store.Select (selectAll)
import Type.Proxy (Proxy(..))

data Action
  = Initialize
  | Receive (Connected Store.Store Unit)
  | StartNewLedger
  | CreateNewLedgerResult CreateLedger.Output
  | OpenLedger String

type State =
  { session :: Maybe Session
  , ledgers :: Maybe (Array Ledger)
  , showNewLedger :: Boolean
  }

type ChildSlots =
  ( createLedger :: H.Slot (Const Void) CreateLedger.Output Unit
  )

homeComponent
  :: forall q o m
   . MonadStore Store.Action Store.Store m
  => MonadAff m
  => MonadNavigate Routes.Route m
  => MonadApiClient m
  => MonadLog m
  => MonadNow m
  => H.Component q Unit o m
homeComponent = connect selectAll $ H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval
      { initialize = Just Initialize
      , receive = Just <<< Receive
      , handleAction = handleAction
      }
    }
  where
  initialState :: Connected Store.Store Unit -> State
  initialState { context: { session, ledgers } } = { session, ledgers, showNewLedger: false }
  handleAction :: Action -> H.HalogenM State Action ChildSlots o m Unit
  handleAction = case _ of
    Initialize -> do
      state <- H.get
      case state of
        { session: Just _, ledgers: Nothing } -> do
          logDebug "refreshing ledger list"
          refreshLedgerList
        _ -> pure unit
    Receive { context: { session, ledgers } } -> do
      logDebug "receiving new stuff"
      H.modify_ _ { session = session, ledgers = ledgers }
      handleAction Initialize
    OpenLedger ledgerId -> do
      navigate $ Routes.Ledger ledgerId Routes.LedgerDashboard
    StartNewLedger -> H.modify_ _ { showNewLedger = true }
    CreateNewLedgerResult (CreateLedger.LedgerCreationRequested name) -> do
      createLedger name
      H.modify_ _ { showNewLedger = false }
    CreateNewLedgerResult (CreateLedger.LedgerCreationCancelled) -> do
      H.modify_ _ { showNewLedger = false }

  render :: State -> H.ComponentHTML Action ChildSlots m
  render { session, ledgers, showNewLedger } =
     HH.div
      [ css "text-center min-h-screen w-screen flex flex-col justify-center items-center" ]
      [ HH.div
        [ css "max-w-lg p-4"]
        [ HH.h1
            [ css "text-4xl text-center w-full" ]
            [ HH.text "Empire Builder" ]
        , HH.h2
            [ css "mb-12 text-gray-400" ]
            [ HH.text "Blah blah blah blah." ]
        , case session of
          Just _ -> case ledgers of
            Just ledgerList ->
              HH.ul [ css "flex flex-col gap-2" ] $
                if showNewLedger
                then
                  [ HH.li_ [ HH.slot (Proxy :: _ "createLedger") unit CreateLedger.createLedgerComponent unit CreateNewLedgerResult ]
                  ]
                else
                  [ btn_ "New Ledger" StartNewLedger
                  ]
                  <>
                  ( ledgerList <#> \ledger ->
                    HH.li_
                      [ btn_ ledger.result.name $ OpenLedger ledger.ledgerId ]
                  )
            Nothing ->
              HH.div_ [ HH.text "Loading..."]
          Nothing -> link_ "Login with Github" "/api/auth/github"
        ]
      ]