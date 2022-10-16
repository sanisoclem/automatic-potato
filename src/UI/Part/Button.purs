module AP.UI.Part.Button  where

import Prelude

import AP.UI.Component.HTML.Utils (css)
import DOM.HTML.Indexed as I
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

btn :: forall action. String -> action -> Array (HH.IProp I.HTMLa action) -> H.ComponentHTML action _ _
btn label action additionalAttribs =
  HH.a
    ( [ HE.onClick \_ -> action
      , css "inline-block tracking-wide px-4 py-2 hover:text-white rounded border-transparent border-solid border-2 active:text-white link:text-white hover:border-green-500"
      ]
      <> additionalAttribs
    )
    [ HH.text label ]

btn_ :: forall action. String -> action -> H.ComponentHTML action _ _
btn_ label action = btn label action []

btnSubmit :: String -> Array _ -> H.ComponentHTML _ _ _
btnSubmit label additionalAttribs =
  HH.button
    ( [ HP.type_ HP.ButtonSubmit
      , css "inline-block tracking-wide px-4 py-2 hover:text-white rounded border-transparent border-solid border-2 active:text-white link:text-white hover:border-green-500"
      ]
      <> additionalAttribs
    )
    [ HH.text label]

btnSubmit_ :: String -> H.ComponentHTML _ _ _
btnSubmit_ = btnSubmit <@> []

link :: forall action. String -> String -> Array (HH.IProp I.HTMLa action) -> H.ComponentHTML action _ _
link label href additionalAttribs =
  HH.a
    ( [ HP.href href
      , css "inline-block tracking-wide px-4 py-2 hover:text-white active:text-white link:text-white"
      ]
      <> additionalAttribs
    )
    [ HH.text label ]

link_ :: forall action. String -> String -> H.ComponentHTML action _ _
link_ label href = link label href []

linkAction :: forall action. String -> Array (HH.IProp I.HTMLa action) -> Array (H.ComponentHTML action _ _) -> H.ComponentHTML action _ _
linkAction additionalCss additionalAttribs inner  =
  HH.a
    ( [ css $ "inline-block cursor-pointer tracking-wide hover:text-green active:text-green link:text-white" <> additionalCss
      ]
      <> additionalAttribs
    )
    inner

linkAction_ :: forall action. String -> action -> H.ComponentHTML action _ _
linkAction_ label action = linkAction mempty [ HE.onClick \_ -> action ] [ HH.text label ]
