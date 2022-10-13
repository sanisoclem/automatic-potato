module AP.UI.Part.Button  where

import Prelude

import AP.UI.Component.HTML.Utils (css)
import DOM.HTML.Indexed as I
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

linkBtn :: forall action. String -> action -> Array (HH.IProp I.HTMLa action) -> H.ComponentHTML action _ _
linkBtn label action additionalAttribs =
  HH.a
    ( [ HE.onClick \_ -> action
      , css "inline-block tracking-wide px-4 py-2 hover:text-white rounded border-transparent border-solid border-2 active:text-white link:text-white hover:border-green-500w-full block tracking-wide px-4 py-2 hover:text-white rounded border-transparent border-solid border-2 active:text-white link:text-white hover:border-green-500"
      ]
      <> additionalAttribs
    )
    [ HH.text label ]

linkBtn_ :: forall action. String -> action -> H.ComponentHTML action _ _
linkBtn_ label action = linkBtn label action []

btnSubmit :: String -> Array _ -> H.ComponentHTML _ _ _
btnSubmit label additionalAttribs =
  HH.button
    ( [ HP.type_ HP.ButtonSubmit
      , css "inline-block tracking-wide px-4 py-2 hover:text-white rounded border-transparent border-solid border-2 active:text-white link:text-white hover:border-green-500w-full block tracking-wide px-4 py-2 hover:text-white rounded border-transparent border-solid border-2 active:text-white link:text-white hover:border-green-500"
      ]
      <> additionalAttribs
    )
    [ HH.text label]

btnSubmit_ :: String -> H.ComponentHTML _ _ _
btnSubmit_ = btnSubmit <@> []
