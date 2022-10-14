module AP.UI.Component.Ledger.EditAccount where

import Prelude

import AP.Domain.Ledger.Identifiers (AccountId(..), AccountType(..), Denomination(..), unAccountId)
import AP.UI.Component.HTML.Utils (css)
import AP.UI.Form.Validation as V
import Data.Const (Const)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect.Aff.Class (class MonadAff)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

type Form :: (Type -> Type -> Type -> Type) -> Row Type
type Form f =
  ( name     :: f String V.FormError String
  , accountType :: f AccountType V.FormError AccountType
  , denomination :: f Denomination V.FormError Denomination
  --              input  error  output
  )


type Input = Maybe AccountId

type Output = Unit

type Query :: forall k. k -> Type
type Query = Const Void

data Action
  = Receive FormContext
  | Eval FormlessAction

type FormContext = F.FormContext (Form F.FieldState) (Form (F.FieldAction Action)) Input Action
type FormlessAction = F.FormlessAction (Form F.FieldState)

editAccountComponent
  :: forall m
   . MonadAff m
  => H.Component Query Input Output m
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
    , denomination: Currency mempty
    }
  render :: FormContext -> H.ComponentHTML Action () m
  render { input } =
    HH.div_
      [ HH.div
        [ css "mx-auto p-4 gap-y-8 max-w-md flex flex-col justify-center items-center" ]
        [ HH.h2_
            [ HH.text $ "Edit account!!!" <> (fromMaybe "NEW" (unAccountId <$> input))  ]
        ]
      ]
  handleAction :: Action -> H.HalogenM FormContext Action () (F.FormOutput (Form F.FieldState) Output) m Unit
  handleAction = case _ of
    Receive ctx -> H.put ctx
    Eval action -> F.eval action
  handleQuery :: forall a. F.FormQuery Query _ _ _ a -> H.HalogenM _ _ _ _ m (Maybe a)
  handleQuery =
    let
      onSubmit :: { | Form F.FieldOutput } -> H.HalogenM _ _ _ _ m Unit
      onSubmit x = do
        pure unit
      validation :: { | Form F.FieldValidation }
      validation =
        { name: V.required >=> V.minLength 3
        , accountType: pure
        , denomination: pure
        }
    in
    F.handleSubmitValidate onSubmit F.validate validation

