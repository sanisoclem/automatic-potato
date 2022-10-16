module AP.UI.Component.Ledger.Transactions (transactionsComponent) where

import Prelude

import AP.Domain.Ledger.Identifiers (AccountId(..))
import AP.UI.Capability.ApiClient (Account, Ledger, NewTransaction, Transaction)
import AP.UI.Component.HTML.Utils (css)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

type Input =
  { ledger :: Ledger
  , account :: Account
  }

type State =
  { ledger :: Ledger
  , account :: Account
  , activeTransaction :: EditedTransaction
  , transactions :: Array Transaction }

data EditedTransaction
  = New NewTransaction
  | Existing Transaction
  | None

data Action
  = Initialize
  | Receive Input
  | StartNewTransaction
  | StartEditTransaction Transaction
  | EditTransaction Transaction
  | NewTransaction NewTransaction

transactionsComponent
  :: forall q o m
   . H.Component q Input o m
transactionsComponent =
  H.mkComponent
    { initialState: \{ ledger, account } -> { ledger, account, activeTransaction: None, transactions: [] }
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        , receive = Just <<< Receive
        }
    }
  where
  render :: State -> H.ComponentHTML Action () m
  render state =
    HH.div_
      [ HH.div
        [ css "mx-auto p-4 gap-y-8 max-w-md flex flex-col justify-center items-center" ]
        [ HH.h2_
            [ HH.text $ "Transactions for " <> state.account.name]
        ]
      ]
  handleAction :: Action -> H.HalogenM State Action () o m Unit
  handleAction = case _ of
    Initialize -> do
      pure unit
    Receive { ledger, account } ->
      H.modify_ _ { ledger = ledger, account = account }
    -- StartNewTransaction
    -- StartEditTransaction Transaction
    -- EditTransaction Transaction
    -- NewTransaction NewTransaction
    _ -> pure unit