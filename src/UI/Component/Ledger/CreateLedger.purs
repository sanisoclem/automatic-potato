module AP.UI.Component.Ledger.CreateLedger
  ( createLedgerComponent
  , Output (..)
  )
  where

import Prelude

import AP.UI.Component.HTML.Utils (css)
import AP.UI.Form.Validation as V
import AP.UI.Part.Button (btnSubmit_, btn_)
import AP.UI.Part.Form (inputText_)
import Data.Const (Const)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

type Form :: (Type -> Type -> Type -> Type) -> Row Type
type Form f =
  ( name     :: f String V.FormError String
  --              input  error  output
  )

type Input = Unit
data Output
  = LedgerCreationRequested String
  | LedgerCreationCancelled

type Query :: forall k. k -> Type
type Query = Const Void

type State = FormContext

data Action
  = Receive FormContext
  | Eval FormlessAction
  | CancelNewLedger

type FormContext = F.FormContext (Form F.FieldState) (Form (F.FieldAction Action)) Input Action
type FormlessAction = F.FormlessAction (Form F.FieldState)

createLedgerComponent
  :: forall m
   . MonadAff m
  => H.Component Query Input Output m
createLedgerComponent =
  F.formless { liftAction: Eval } mempty $ H.mkComponent
    { initialState: identity
    , render
    , eval: H.mkEval H.defaultEval
        { receive = Just <<< Receive
        , handleAction = handleAction
        , handleQuery = handleQuery
        }
    }
  where
  handleAction :: Action -> H.HalogenM State Action () (F.FormOutput (Form F.FieldState) Output) m Unit
  handleAction = case _ of
    CancelNewLedger ->
      F.raise LedgerCreationCancelled
    Receive ctx -> H.put ctx
    Eval action -> F.eval action
  handleQuery :: forall a. F.FormQuery Query _ _ _ a -> H.HalogenM _ _ _ _ m (Maybe a)
  handleQuery =
    let
      onSubmit x = do
        F.raise $ LedgerCreationRequested x.name
    in
    F.handleSubmitValidate onSubmit F.validate
      { name: V.required >=> V.minLength 3
      }
  render :: State -> H.ComponentHTML Action () m
  render form =
   HH.form
      [ HE.onSubmit form.formActions.handleSubmit
      , css "flex gap-2 flex-col"
      ]
      [ inputText_ form.fields.name.value form.actions.name "Name"
      , case form.fields.name.result of
          Just (Left error) -> HH.text $ V.errorToString error
          _ -> HH.text ""
      , btnSubmit_ "Create"
      , btn_ "Cancel" CancelNewLedger
      ]