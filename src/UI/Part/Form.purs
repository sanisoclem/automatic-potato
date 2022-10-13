module AP.UI.Part.Form where

import Prelude

import AP.UI.Component.HTML.Utils (css)
import DOM.HTML.Indexed as I
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP


inputText :: forall action. String -> F.FieldAction action _ _ _  -> String -> Array (HH.IProp I.HTMLinput action) -> H.ComponentHTML action _ _
inputText value fieldActions placeholder additionalProps =
  HH.input $
    [ css "text-gray-800 p-2 mx-2"
    , HP.type_ HP.InputText
    , HP.placeholder placeholder
    , HP.value value
    , HE.onValueInput fieldActions.handleChange
    , HE.onBlur fieldActions.handleBlur
    ]
    <> additionalProps

inputText_ v a p = inputText v a p []