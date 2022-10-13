module AP.UI.Component.Home
  ( homeComponent
  )
  where

import Prelude

import AP.Capability.ApiClient (class MonadApiClient, Ledger, createLedger, getLedgers)
import AP.UI.Capability.Navigate (class MonadNavigate, navigate)
import AP.UI.Component.HTML.Utils (css)
import AP.UI.Form.Validation as V
import AP.UI.Part.Button (btnSubmit_, linkBtn_)
import AP.UI.Part.Form (inputText_)
import AP.UI.Route as Routes
import Data.Const (Const)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type Form :: (Type -> Type -> Type -> Type) -> Row Type
type Form f =
  ( name     :: f String V.FormError String
  --              input  error  output
  )

type Input = Unit
type Output = Unit
type Query :: forall k. k -> Type
type Query = Const Void

type State =
  { form:: FormContext
  , ledgerList :: LedgerListState
  }
data LedgerListState
  = Loading
  | Loaded
    { ledgers :: Array Ledger
    , showNewLedger :: Boolean
    }

data Action
  = Initialize
  | Receive FormContext
  | Eval FormlessAction
  | OpenLedger String
  | StartNewLedger
  | CancelNewLedger

type FormContext = F.FormContext (Form F.FieldState) (Form (F.FieldAction Action)) Input Action
type FormlessAction = F.FormlessAction (Form F.FieldState)

homeComponent
  :: forall m
   . MonadApiClient m
  => MonadAff m
  => MonadNavigate Routes.Route m
  => H.Component Query Input Output m
homeComponent =
  F.formless { liftAction: Eval } mempty $ H.mkComponent
    { initialState: \ctx -> { form: ctx, ledgerList: Loading }
    , render
    , eval: H.mkEval H.defaultEval
        { initialize = Just Initialize
        , receive = Just <<< Receive
        , handleAction = handleAction
        , handleQuery = handleQuery
        }
    }
  where
  handleAction :: Action -> H.HalogenM State Action () (F.FormOutput (Form F.FieldState) Output) m Unit
  handleAction = case _ of
    Initialize -> do
      ledgers <- getLedgers
      H.modify_ _ { ledgerList = Loaded { ledgers, showNewLedger: false } }
    OpenLedger ledgerId -> do
      navigate $ Routes.Ledger ledgerId Routes.LedgerDashboard
    StartNewLedger -> H.modify_ \s -> case s.ledgerList of
      Loaded x -> s { ledgerList = Loaded x { showNewLedger = true } }
      _ -> s
    CancelNewLedger -> H.modify_ \s -> case s.ledgerList of
      Loaded x -> s { ledgerList = Loaded x { showNewLedger = false } }
      _ -> s
    Receive ctx -> H.modify_ _ { form = ctx }
    Eval action -> F.eval action
  handleQuery :: forall a. F.FormQuery Query _ _ _ a -> H.HalogenM _ _ _ _ m (Maybe a)
  handleQuery =
    let
      onSubmit x = do
        f <- H.gets _.form
        handleAction f.formActions.reset
        createLedger x.name
        H.modify_ _ { ledgerList = Loading }
        ledgers <- getLedgers
        H.modify_ _ { ledgerList = Loaded { ledgers, showNewLedger: false } }
    in
    F.handleSubmitValidate onSubmit F.validate
      { name: V.required >=> V.minLength 3
      }
  render :: State -> H.ComponentHTML Action () m
  render state =
    HH.div
      [ css "text-center min-h-screen w-screen flex flex-col justify-center items-center" ]
      [ HH.div
        [ css "max-w-screen-lg p-4"]
        [ HH.h1
            [ css "text-4xl text-center w-full" ]
            [ HH.text "Empire Builder" ]
        , HH.h2
            [ css "mb-12 text-gray-400" ]
            [ HH.text "Pick a Ledger" ]
        , case state.ledgerList of
          Loading -> renderLoading
          Loaded x -> renderLoaded state x
        ]
      ]
  renderLoading =
    HH.div_ [ HH.text "Loading..."]
  renderLoaded state { ledgers, showNewLedger } =
    HH.ul [ css "flex flex-col gap-2" ] $
      [ HH.li_
        if showNewLedger
        then
          [ HH.form
              [ HE.onSubmit state.form.formActions.handleSubmit ]
              [ inputText_ state.form.fields.name.value state.form.actions.name "Name"
              , case state.form.fields.name.result of
                  Just (Left error) -> HH.text $ V.errorToString error
                  _ -> HH.text ""
              , btnSubmit_ "Create"
              , linkBtn_ "Cancel" CancelNewLedger
              ]
          ]
        else
          [ linkBtn_ "New Ledger" StartNewLedger
          ]
      ]
      <>
      ( ledgers <#> \ledger ->
        HH.li_
          [ linkBtn_ ledger.result.name $ OpenLedger ledger.ledgerId ]
      )