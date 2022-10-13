module AP.UI.Store where

import Prelude

import AP.Capability.ApiClient (Session, Ledger)
import Data.Array ((:))
import Data.Maybe (Maybe(..))
import Routing.PushState (PushStateInterface)

data EnvironmentType = Dev | Prod

derive instance eqEnvironmentType :: Eq EnvironmentType
derive instance ordEnvironmentType :: Ord EnvironmentType

type Store =
  { envType :: EnvironmentType
  , psi :: PushStateInterface
  , session :: Maybe Session
  , debug :: Array Ledger
  }

data Action
  = UseSession Session
  | Logout
  | DebugCreateLedger Ledger

reduce :: Store -> Action -> Store
reduce store = case _ of
  UseSession session -> store { session = Just session }
  Logout -> store { session = Nothing }
  DebugCreateLedger x -> store { debug = x : store.debug }
