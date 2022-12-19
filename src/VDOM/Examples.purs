module VDOM.Examples (CounterMsg, ex1, updateCounter, initialCounterModel, viewCounter) where

import VDOM
import Prelude

type CounterModel =
  { count :: Int
  }

data CounterMsg =
    Increment
  | Decrement

initialCounterModel :: CounterModel
initialCounterModel = { count: 0 }

updateCounter :: CounterMsg -> CounterModel -> CounterModel
updateCounter msg model =
  case msg of
      Increment -> model { count = model.count + 1 }
      Decrement -> model { count = model.count - 1 }

viewCounter :: CounterModel -> Html CounterMsg
viewCounter model =
  el Div
    []
    [ el Div [ Style "color: red;" ] [ text (show model.count) ]
    , el Button [ EventHandler (OnClick Increment) ] [ text "inc" ]
    , el Button [ EventHandler (OnClick Decrement) ] [ text "dec" ]
    ]


ex1 :: Html CounterMsg
ex1 = viewCounter initialCounterModel
