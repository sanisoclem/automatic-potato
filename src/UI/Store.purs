module AP.UI.Store where

import Prelude

import Routing.PushState (PushStateInterface)

data EnvironmentType = Dev | Prod

derive instance eqEnvironmentType :: Eq EnvironmentType
derive instance ordEnvironmentType :: Ord EnvironmentType

type Store =
  { envType :: EnvironmentType
  , psi :: PushStateInterface
  }

data Action
  = Noop

reduce :: Store -> Action -> Store
reduce store = case _ of
  Noop ->
    store
