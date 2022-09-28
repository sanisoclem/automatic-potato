module AP.UI.Component.Ledger.Dashboard (dashboardComponent) where

import Prelude

import AP.UI.Component.HTML.Utils (css)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

type Input = Unit

type State = Unit

data Action = Unit

dashboardComponent
  :: forall q o m
   . H.Component q Input o m
dashboardComponent =
  H.mkComponent
    { initialState: identity
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        }
    }
  where
    render :: State -> H.ComponentHTML Action () m
    render _state =
      HH.div_
        [ HH.div
          [ css "mx-auto p-4 gap-y-8 max-w-md flex flex-col justify-center items-center" ]
          [ HH.h2_
              [ HH.text "Dashboard!!!"]
          ]
        ]
    handleAction :: Action -> H.HalogenM State Action () o m Unit
    handleAction _ = pure unit
