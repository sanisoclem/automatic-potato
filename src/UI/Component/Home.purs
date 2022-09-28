module AP.UI.Component.Home
  ( homeComponent
  )
  where

import Prelude

import AP.Capability.ApiClient (class MonadApiClient, Ledger, getLedgers)
import AP.UI.Capability.Navigate (class MonadNavigate, navigate)
import AP.UI.Component.HTML.Utils (css)
import AP.UI.Route as Routes
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

data State
  = Loading
  | Loaded { ledgers :: Array Ledger }

data Action
  = Initialize
  | OpenLedger String

homeComponent
  :: forall q i o m
   . MonadApiClient m
  => MonadNavigate Routes.Route m
  => H.Component q i o m
homeComponent =
  H.mkComponent
    { initialState: const Loading
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        }
    }
  where
    render :: State -> H.ComponentHTML Action () m
    render state =
      HH.div
        [ css "text-center min-h-screen w-screen flex flex-col justify-center items-center" ]
        [ HH.div
          [ css "max-w-screen-lg p-4"]
          [ HH.h1
              [ css "text-4xl text-center w-full" ]
              [ HH.text "Empire Builder" ]
          , HH.h2
              [ css "mb-12 text-gray-400" ]
              [ HH.text "Pick a Ledger" ]
          , case state of
            Loading ->
              HH.div_ [ HH.text "Loading..."]
            Loaded { ledgers } ->
              HH.ul [ css "flex flex-col gap-2" ] $ ledgers <#> \ledger ->
                HH.li_
                  [ HH.a
                      [ HE.onClick \_ -> OpenLedger ledger.ledgerId
                      , css "block tracking-wide px-4 py-2 hover:text-white rounded border-transparent border-solid border-2 active:text-white link:text-white hover:border-green-500"
                      ]
                      [ HH.text ledger.name ]
                  ]
          ]
        ]

    handleAction :: Action -> H.HalogenM State Action () o m Unit
    handleAction = case _ of
      Initialize -> do
        ledgers <- getLedgers
        H.put $ Loaded { ledgers }
      OpenLedger ledgerId -> do
        navigate $ Routes.Ledger ledgerId Routes.LedgerDashboard