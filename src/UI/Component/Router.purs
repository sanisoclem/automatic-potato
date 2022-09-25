module AP.UI.Component.Router where

import Prelude

import AP.Capability.ApiClient (class MonadApiClient, Session, getSession)
import AP.Capability.Log (class MonadLog)
import AP.Capability.Now (class MonadNow)
import AP.UI.Capability.Navigate (class MonadNavigate)
import AP.UI.Component.HTML.Utils (css)
import AP.UI.Page.Dashboard as Dashboard
import AP.UI.Page.Home as Home
import AP.UI.Route (Route(..))
import AP.UI.Store (EnvironmentType)
import AP.UI.Store as Store

import Control.Monad.Trans.Class (lift)

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

data Query a = Navigate Route a
type Input = Unit

type OpaqueSlot slot = forall query. H.Slot query Void slot

type State =
  { route :: Maybe Route
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
  ( home :: OpaqueSlot Unit
  , dashboard :: OpaqueSlot Unit
  )

component
  :: forall m
   . MonadAff m
  => MonadApiClient m
  => MonadStore Store.Action Store.Store m
  => MonadNow m
  => MonadLog m
  => MonadNavigate Route m
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
      -- don't think this needs to be done
      -- { psi } <- H.get
      -- initialRoute <- hush <<< RD.parse routeCodec <<< _.path <$> liftEffect psi.locationState
      -- void <<< sequence $ navigate <$> initialRoute
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
  render { route, session, env } = case session of
    Just s ->
      HH.div
        [ css "bg-gray-800 min-h-screen w-screen text-white" ]
        [ case route of
          Just r ->
            HH.div_
              [ HH.text $ "Logged in as: " <> s.name
              , case r of
                Home ->
                  HH.slot_ (Proxy :: _ "home") unit Home.component unit
                Dashboard ->
                  HH.slot_ (Proxy :: _ "dashboard") unit Dashboard.component unit
              ]
          Nothing ->
            HH.div
              [ css "mx-auto max-w-md text-center p-20"]
              [ HH.h1_ [ HH.text "Not Found" ]
              , HH.a [ HP.href "/" ] [ HH.text "Home"]
              ]
        ]

    Nothing ->
        HH.div
          [ css "bg-gray-800 w-screen text-white" ]
          [ HH.div
            [ css "mx-auto p-4 min-h-screen gap-y-8 max-w-md flex flex-col justify-center items-center" ]
            [ HH.a
                [ HP.href "/api/auth/github" ]
                [ HH.text "Login with Github" ]
            ]
          ]
