module AP.UI.AppM where

import Prelude

import AP.Capability.ApiClient (class MonadApiClient)
import AP.Capability.Log (class MonadLog)
import AP.Data.Log as Log
import AP.UI.Capability.Navigate (class MonadNavigate, class MonadNavigateAbs)
import AP.UI.Route (Route)
import AP.UI.Route as Route
import AP.UI.Store (Action, EnvironmentType(..), Store)
import AP.UI.Store as Store
import Affjax.ResponseFormat as AXRF
import Affjax.Web as AX
import Data.Argonaut (decodeJson)
import Data.Either (hush)
import Effect.Aff (Aff, Milliseconds(..), delay)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as Console
import Foreign (unsafeToForeign)
import Halogen as H
import Halogen.Store.Monad (class MonadStore, StoreT, getStore, runStoreT)
import Routing.Duplex (print)
import Safe.Coerce (coerce)
import Web.HTML (window)
import Web.HTML.Location (setHref)
import Web.HTML.Window (location)

newtype AppM a = AppM (StoreT Store.Action Store.Store Aff a)

runAppM :: forall q i o. Store.Store -> H.Component q i o AppM -> Aff (H.Component q i o Aff)
runAppM store = runStoreT store Store.reduce <<< coerce

derive newtype instance functorAppM :: Functor AppM
derive newtype instance applyAppM :: Apply AppM
derive newtype instance applicativeAppM :: Applicative AppM
derive newtype instance bindAppM :: Bind AppM
derive newtype instance monadAppM :: Monad AppM
derive newtype instance monadEffectAppM :: MonadEffect AppM
derive newtype instance monadAffAppM :: MonadAff AppM
derive newtype instance monadStoreAppM :: MonadStore Action Store AppM

instance MonadLog AppM where
  logMessage log = do
    { envType } <- getStore
    liftEffect case envType, Log.level log of
      Prod, Log.Debug -> pure unit
      _, _ -> Console.log $ Log.message log

instance MonadNavigate Route AppM where
  navigate x = do
    { psi } <- getStore
    liftEffect $ psi.pushState (unsafeToForeign unit) $ print Route.routeCodec x

instance MonadNavigateAbs AppM where
  navigateAbs href = liftEffect $ setHref href =<< location =<< window

instance MonadApiClient AppM where
  getSession = do
    response <- liftAff $ AX.get AXRF.json ("/api/session")
    pure  $ hush <<< decodeJson <<< _.body =<< hush response
  getLedgers = do
    liftAff <<< delay $ Milliseconds 1000.0
    pure
      [ { ledgerId: "emptyLedger", name: "Empty Ledger"}
      , { ledgerId: "simpleLedger", name: "Simple Ledger"}
      , { ledgerId: "complexLedger", name: "Complex Ledger"}
      ]