module AP.UI.Component.Router where

import Prelude

import AP.Capability.Log (class MonadLog)
import AP.Capability.Navigate (class MonadNavigate, navigate)
import AP.Capability.Now (class MonadNow)
import AP.UI.Page.Dashboard as Dashboard
import AP.UI.Page.Home as Home
import AP.UI.Route (Route(..), routeCodec)
import AP.UI.Store as Store
import Data.Either (hush)
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence)
import Effect.Aff.Class (class MonadAff)
import Halogen (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore)
import Halogen.Store.Select (select)
import Routing.Duplex as RD
import Routing.PushState (PushStateInterface)
import Type.Proxy (Proxy(..))

data Query a = Navigate Route a
type Input = Unit

type OpaqueSlot slot = forall query. H.Slot query Void slot

type State =
  { route :: Maybe Route
  , psi :: PushStateInterface
  }
deriveState :: Connected PushStateInterface Input -> State
deriveState { context } = { route: Nothing, psi: context }


data Action
  = Initialize

type ChildSlots =
  ( home :: OpaqueSlot Unit
  , dashboard :: OpaqueSlot Unit
  )

component
  :: forall m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => MonadNow m
  => MonadLog m
  => MonadNavigate Route m
  => H.Component Query Input Void m
component = connect (select (const <<< const $ false) _.psi) $ H.mkComponent
  { initialState: deriveState
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleQuery = handleQuery
      , handleAction = handleAction
      , initialize = Just Initialize
      }
  }
  where
  handleAction :: Action -> H.HalogenM State Action ChildSlots Void m Unit
  handleAction = case _ of
    Initialize -> do
      { psi } <- H.get
      -- first we'll get the route the user landed on
      initialRoute <- hush <<< RD.parse routeCodec <<< _.path <$> liftEffect psi.locationState
      -- then we'll navigate to the new route (also setting the hash)
      void <<< sequence $ navigate <$> initialRoute
      --navigate $ fromMaybe Home initialRoute

  handleQuery :: forall a. Query a -> H.HalogenM State Action ChildSlots Void m (Maybe a)
  handleQuery = case _ of
    Navigate dest a -> do
      { route } <- H.get
      -- don't re-render unnecessarily if the route is unchanged
      when (route /= Just dest) do
        H.modify_ _ { route = Just dest }
      pure (Just a)

  render :: State -> H.ComponentHTML Action ChildSlots m
  render { route } = case route of
    Just r -> case r of
      Home ->
        HH.slot_ (Proxy :: _ "home") unit Home.component unit
      Dashboard ->
        HH.slot_ (Proxy :: _ "dashboard") unit Dashboard.component unit
    Nothing ->
      HH.div_ [ HH.text "Oh no! That page wasn't found." ]
