module AP.UI.Main where

import Prelude

import Data.Maybe (Maybe(..))
import AP.UI.AppM (runAppM)
import AP.UI.Component.Router as Router
import AP.UI.Route (routeCodec)
import AP.UI.Store (EnvironmentType(..), Store)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Halogen (liftEffect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Routing.Duplex (parse)
import Routing.PushState (makeInterface, matchesWith)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  let envType = Dev
  psi <- liftEffect $ makeInterface

  let
    initialStore :: Store
    initialStore =
      { envType
      , psi
      }

  rootComponent <- runAppM initialStore Router.component

  halogenIO <- runUI rootComponent unit body
  void $ liftEffect $ matchesWith (parse routeCodec) (handler halogenIO) psi

  where
    handler halogenIO old new = when (old /= Just new) $ launchAff_ do
      _response <- halogenIO.query $ H.mkTell $ Router.Navigate new
      pure unit
