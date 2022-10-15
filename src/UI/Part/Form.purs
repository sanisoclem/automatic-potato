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


type RadioGroup action input output =
  { label :: String
  , state :: F.FieldState input Void output
  , action :: F.FieldAction action input Void output
  , options ::
      Array
        { option :: input
        , render :: String
        , props :: Array (HP.IProp I.HTMLinput action)
        }
  }

radioGroup
  :: forall input output action slots m
   . Eq input
  => RadioGroup action input output
  -> H.ComponentHTML action slots m
radioGroup { label, state, action, options } =
  HH.div_
    [ HH.label_ [ HH.text label ]
    , HH.fieldset_ $ options <#> \{ option, render, props } ->
        HH.label_
          [ HH.input $ flip append props
              [ HP.type_ HP.InputRadio
              , HP.name action.key
              , HP.checked (state.value == option)
              , HE.onChange (\_ -> action.handleChange option)
              , HE.onBlur action.handleBlur
              ]
          , HH.text render
          ]
    ]