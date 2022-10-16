module AP.UI.Component.Router where

import Prelude

import AP.UI.Capability.ApiClient (class MonadApiClient, Session, getSession)
import AP.Capability.Log (class MonadLog)
import AP.Capability.Now (class MonadNow)
import AP.UI.Capability.Navigate (class MonadNavigate)
import AP.UI.Component.HTML.Utils (css)
import AP.UI.Component.Home as Component
import AP.UI.Component.Ledger.Router as Component.Ledger
import AP.UI.Component.Utility (OpaqueSlot)
import AP.UI.Part.Button (link_)
import AP.UI.Route (toHref)
import AP.UI.Route as Routes
import AP.UI.Store (EnvironmentType)
import AP.UI.Store as Store
import Control.Monad.Trans.Class (lift)
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore, updateStore)
import Halogen.Store.Select (selectAll)
import Routing.PushState (PushStateInterface)
import Type.Proxy (Proxy(..))

data Query a = Navigate Routes.Route a

data Test = Tes2t String String

type Input = Unit

type State =
  { route :: Maybe Routes.Route
  , psi :: PushStateInterface
  , session :: Maybe Session
  , env :: EnvironmentType
  }
deriveState :: Connected Store.Store Input -> State
deriveState { context } = { route: Nothing, psi: context.psi, session: context.session, env: context.envType }

data Action
  = Initialize
  | Receive (Connected Store.Store Input)

type ChildSlots =
  ( home :: H.Slot (Const Void) Unit Unit
  , ledger :: OpaqueSlot Unit
  )

component
  :: forall m
   . MonadAff m
  => MonadApiClient m
  => MonadStore Store.Action Store.Store m
  => MonadNow m
  => MonadLog m
  => MonadNavigate Routes.Route m
  => H.Component Query Input Void m
component = connect selectAll $ H.mkComponent
  { initialState: deriveState
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleQuery = handleQuery
      , handleAction = handleAction
      , initialize = Just Initialize
      , receive = Just <<< Receive
      }
  }
  where
  handleAction :: Action -> H.HalogenM State Action ChildSlots Void m Unit
  handleAction = case _ of
    Initialize -> do
      session <- getSession
      void <<< sequence $ lift <<< updateStore <<< Store.UseSession <$> session
      pure unit
    Receive { context } -> do
      H.modify_  _ { session = context.session, env = context.envType }

  handleQuery :: forall a. Query a -> H.HalogenM State Action ChildSlots Void m (Maybe a)
  handleQuery = case _ of
    Navigate dest a -> do
      { route } <- H.get
      -- don't re-render unnecessarily if the route is unchanged
      when (route /= Just dest) do
        H.modify_ _ { route = Just dest }
      pure (Just a)

  render :: State -> H.ComponentHTML Action ChildSlots m
  render { route, session } =
    HH.div
      [ css "bg-gray-900 text-white" ]
      [ case { route, session } of
          { route: Just (Routes.Home) } ->
            HH.slot_ (Proxy :: _ "home") unit Component.homeComponent unit
          { route: Just _r, session: Nothing } ->
            HH.slot_ (Proxy :: _ "home") unit Component.homeComponent unit -- TODO: return url
          { route: Just (Routes.Ledger ledgerId l), session: Just _ } ->
            HH.slot_ (Proxy :: _ "ledger") unit Component.Ledger.routerComponent { ledgerId, route: l }
          { route: Nothing } ->
            HH.div
              [ css "mx-auto max-w-md text-center p-20"]
              [ HH.h1_ [ HH.text "Not Found" ]
              , HH.a [ HP.href $ toHref Routes.Home ] [ HH.text "Home"]
              ]
      ]
