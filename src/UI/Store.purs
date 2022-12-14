module AP.UI.Store where

import Prelude

import AP.UI.Capability.ApiClient (Ledger, Session, Balances)
import Data.Map (Map, insert)
import Data.Maybe (Maybe(..))
import Routing.PushState (PushStateInterface)

data EnvironmentType = Dev | Prod

derive instance eqEnvironmentType :: Eq EnvironmentType
derive instance ordEnvironmentType :: Ord EnvironmentType

type Store =
  { envType :: EnvironmentType
  , psi :: PushStateInterface
  , session :: Maybe Session
  , ledgers :: Maybe (Array Ledger)
  , balances :: Map String Balances
  }

data Action
  = UseSession Session
  | Logout
  | UpdateLedgers (Array Ledger)
  | UpdateBalances String Balances

reduce :: Store -> Action -> Store
reduce store = case _ of
  UseSession session -> store { session = Just session }
  Logout -> store { session = Nothing }
  UpdateLedgers x -> store { ledgers = Just x }
  UpdateBalances ledgerId bal -> store { balances = insert ledgerId bal store.balances }
