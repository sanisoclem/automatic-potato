module AP.UI.Component.Ledger.EditAccount where

import Prelude

import AP.UI.Capability.ApiClient (class MonadApiClient, Ledger, createAccount)
import AP.UI.Capability.Navigate (class MonadNavigate, navigate)
import AP.Domain.Ledger.Identifiers (AccountId, AccountType(..))
import AP.UI.Component.HTML.Utils (css)
import AP.UI.Form.Validation as V
import AP.UI.Part.Button (btnSubmit_)
import AP.UI.Part.Form (inputText_, radioGroup)
import AP.UI.Route as Routes
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Effect.Aff.Class (class MonadAff)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

type Form :: (Type -> Type -> Type -> Type) -> Row Type
type Form f =
  ( name     :: f String V.FormError String
  , accountType :: f AccountType Void AccountType
  , denomination :: f String V.FormError String
  --              input  error  output
  )

type Input =
  { ledger:: Ledger
  , accountId:: Maybe AccountId
  }

data Action
  = Receive FormContext
  | Eval FormlessAction

type FormContext = F.FormContext (Form F.FieldState) (Form (F.FieldAction Action)) Input Action
type FormlessAction = F.FormlessAction (Form F.FieldState)

editAccountComponent
  :: forall m q o
   . MonadAff m
  => MonadApiClient m
  => MonadNavigate Routes.Route m
  => H.Component q Input o m
editAccountComponent =
  F.formless { liftAction: Eval } initialValues $ H.mkComponent
    { initialState: \c -> c
    , render
    , eval: H.mkEval H.defaultEval
        { receive = Just <<< Receive
        , handleAction = handleAction
        , handleQuery = handleQuery
        }
    }
  where
  initialValues :: { | Form F.FieldInput }
  initialValues =
    { name: mempty
    , accountType: Asset
    , denomination: mempty
    }
  handleAction :: Action -> H.HalogenM FormContext Action () (F.FormOutput (Form F.FieldState) o) m Unit
  handleAction = case _ of
    Receive ctx -> H.put ctx
    Eval action -> F.eval action
  handleQuery :: forall a. F.FormQuery q _ _ _ a -> H.HalogenM _ _ _ _ m (Maybe a)
  handleQuery =
    let
      onSubmit :: { | Form F.FieldOutput } -> H.HalogenM _ _ _ _ m Unit
      onSubmit x = do
        input <- H.gets _.input
        case input.accountId of
          Just _ -> pure unit
          Nothing -> createAccount
            { ledgerId: input.ledger.ledgerId
            , name: x.name
            , denomination: x.denomination
            , accountType: x.accountType
            }
        navigate $ Routes.Ledger input.ledger.ledgerId $ Routes.LedgerDashboard -- TODO: get accountId of new account and redirect to txn page
      validation :: { | Form F.FieldValidation }
      validation =
        { name: V.required >=> V.minLength 3
        , accountType: pure
        , denomination: V.required >=> V.minLength 3
        }
    in
    F.handleSubmitValidate onSubmit F.validate validation
  render :: FormContext -> H.ComponentHTML Action () m
  render { formActions, fields, actions, input } =
    HH.div_
      [ HH.div
        [ css "" ]
        [ HH.h2_
            [ HH.text <<< fromMaybe "New Account" $ const "Edit Account" <$> input.accountId ]
        ]
      , HH.form
          [ HE.onSubmit formActions.handleSubmit ]
          [ inputText_ fields.name.value actions.name "Name"
          , case fields.name.result of
              Just (Left error) -> HH.text $ V.errorToString error
              _ -> HH.text ""
          , radioGroup
              { label: "Account Type"
              , options:
                  [ { option: Asset, render: "Asset", props: [] }
                  , { option: Liability, render: "Liability", props: [] }
                  , { option: Income, render: "Income", props: [] }
                  , { option: Expense, render: "Expense", props: [] }
                  ]
              , state: fields.accountType
              , action: actions.accountType
              }
          , inputText_ fields.denomination.value actions.denomination "Denomination"
          , case fields.denomination.result of
              Just (Left error) -> HH.text $ V.errorToString error
              _ -> HH.text ""
          , btnSubmit_ <<< fromMaybe "Create" $ const "Update" <$> input.accountId
          ]
      ]

