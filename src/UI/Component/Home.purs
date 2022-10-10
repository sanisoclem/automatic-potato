module AP.UI.Component.Home
  ( homeComponent
  )
  where

import Prelude

import AP.Capability.ApiClient (class MonadApiClient, Ledger, getLedgers)
import AP.UI.Capability.Navigate (class MonadNavigate, navigate)
import AP.UI.Component.HTML.Utils (css)
import AP.UI.Form.Validation as V
import AP.UI.Route as Routes
import Data.Array as Array
import Data.Const (Const)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type Form :: (Type -> Type -> Type -> Type) -> Row Type
type Form f =
  ( name     :: f String String String
  --              input  error  output
  )

type Input = Unit
type Output = Unit
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
  | NewLedger String

type FormContext = F.FormContext (Form F.FieldState) (Form (F.FieldAction Action)) Input Action
type FormlessAction = F.FormlessAction (Form F.FieldState)

homeComponent
  :: forall m
   . MonadApiClient m
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
      x -> s
    NewLedger x -> do
      pure unit
    Receive ctx -> H.modify_ _ { context = ctx }
    Eval action -> F.eval action
  handleQuery :: forall a. Query a -> H.HalogenM State Action () (F.FormOutput (Form F.FieldState) Output) m (Maybe a)
  handleQuery =
    F.handleSubmitValidate F.raise F.validate
      { name: V.required >=> V.minLength 3
      }

  render :: State -> H.ComponentHTML Action () m
  render state =
    let lnkItemCss = "block tracking-wide px-4 py-2 hover:text-white rounded border-transparent border-solid border-2 active:text-white link:text-white hover:border-green-500" in
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
          Loading ->
            HH.div_ [ HH.text "Loading..."]
          Loaded { ledgers, showNewLedger } ->
            HH.ul [ css "flex flex-col gap-2" ] $ Array.cons
              ( HH.li_
                if showNewLedger
                then
                  [ HH.input
                      [ HP.type_ HP.InputText
                      , HP.placeholder "Name"
                      , HP.value state.form.fields.name.value
                      , HE.onValueInput state.form.actions.name.handleChange
                      , HE.onBlur state.form.actions.name.handleBlur
                      ]
                  , case state.form.fields.name.result of
                      Just (Left error) -> HH.text error
                      _ -> HH.text ""
                  ]
                else
                  [ HH.a
                      [ HE.onClick \_ -> StartNewLedger
                      , css lnkItemCss ]
                      [ HH.text "New Ledger" ]
                  ]
              )
              ( ledgers <#> \ledger ->
                HH.li_
                  [ HH.a
                    [ HE.onClick \_ -> OpenLedger ledger.ledgerId
                    , css lnkItemCss
                    ]
                    [ HH.text ledger.name ]
                  ])
        ]
      ]

