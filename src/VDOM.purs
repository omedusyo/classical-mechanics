module VDOM where

import Prelude
import Data.Map as Map
import Data.List as List
import Data.Foldable (foldl)
type Map = Map.Map
type List = List.List


data Html msg =
    Text String
  -- TODO: How to represent attributes so that they are unordered, yet diffable?
  --       I will need to compute differences and intersection of two attribute maps.
  --
  --       What to do about the events?
  --       Holy shit! The messages need to be comparable for equality...
  --       But what about something like onClick functions?
  --       I have event producers...
  --       How do I know if I need to patch an event producer?
  --
  --       Doesn't really make sense in a purely functional language...
  --       Do I need to deal with identity here?
  | El Tag SimpleAttributes (Array (EventHandler msg)) (Array (Html msg))

text :: forall msg . String -> Html msg
text = Text

el :: forall msg . Tag -> Array (Attribute msg) -> Array (Html msg) -> Html msg
el tag attrs children =
  let internalAttrs = attributesToInternalAttributes attrs
   in
   El tag internalAttrs.simple (internalAttrs.eventHandlers # List.toUnfoldable) children

type AttributeName = String

type AttributeValue = String

type Css = String


data Attribute msg =
    Style Css
  | EventHandler (EventHandler msg)
  | Id String
  | ClassName String

attributesToInternalAttributes :: forall msg . Array (Attribute msg) -> InternalAttributes msg
attributesToInternalAttributes attributes =
  -- Note that the order of eventHandlers is reversed
  attributes
    # foldl
        (\internalAttrs attr ->
          case attr of
            Style cssStr ->
              internalAttrs { simple = internalAttrs.simple # Map.insert "style" cssStr }
            Id idStr ->
              internalAttrs { simple = internalAttrs.simple # Map.insert "id" idStr }
            ClassName classStr ->
              internalAttrs { simple = internalAttrs.simple # Map.insert "class" classStr }
            EventHandler handler ->
              internalAttrs { eventHandlers = List.Cons handler internalAttrs.eventHandlers }
        )
        { simple: Map.empty, eventHandlers: List.Nil }


type InternalAttributes msg =
  { simple :: SimpleAttributes
  , eventHandlers :: List (EventHandler msg)
  }

type SimpleAttributes =
  Map AttributeName AttributeValue

data Tag =
    H1
  | H2
  | Div
  | Input
  | Span
  | Ul
  | Li
  | Button

derive instance eqTag :: Eq Tag


data EventHandler msg =
    OnInputChange (String -> msg)
  | OnClick msg
