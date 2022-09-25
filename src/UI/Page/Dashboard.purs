module AP.UI.Page.Dashboard
  ( component
  )
  where

import Prelude

import AP.Capability.Log (class MonadLog, logWarn)
import AP.Capability.Now (class MonadNow)
import AP.UI.Component.HTML.Utils (css)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

type State
  = { count :: Int }

data Action
  = Increment
  | Initialize

component
  :: forall q i o m
   . MonadLog m
  => MonadNow m
  => H.Component q i o m
component =
  H.mkComponent
    { initialState: \_ -> { count: 0 }
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        }
    }

render :: forall cs m. State -> H.ComponentHTML Action cs m
render state =
  HH.div_
    [ HH.div
      [ css "mx-auto p-4 gap-y-8 max-w-md flex flex-col justify-center items-center" ]
      [ HH.h2_
          [ HH.text "Dashboard!!!"]
      , HH.p_
          [ HH.text $ "You clicked " <> show state.count <> " times" ]
      , HH.button
          [ HE.onClick \_ -> Increment ]
          [ HH.text "Click me" ]
      ]
    ]


handleAction
  :: forall cs o m
   . MonadLog m
  => MonadNow m
  => Action
  -> H.HalogenM State Action cs o m Unit
handleAction = case _ of
  Increment -> H.modify_ \st -> st { count = st.count + 1 }
  Initialize -> do
    logWarn "This is a test"
