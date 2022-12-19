module VDOM.Render (fromVDOM, fromComponent) where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.Maybe (Maybe(..))
import Data.Array as Array
import Data.List as List
import Data.Map as Map
import Data.Set as Set
import Data.Tuple as Tuple
import DOM as DOM
import Data.Traversable ( traverse )
import VDOM ( Html(..), Tag(..), Attribute(..), EventHandler(..), SimpleAttributes, AttributeName, AttributeValue )
import Store as Store
import Duration as Duration
import Effect.Extra.Timer as Timer
import Duration ( Duration )
type Map = Map.Map
type List = List.List
type Tuple = Tuple.Tuple

type DOM = DOM.DOM
type Store = Store.Store

tagToString :: Tag -> String
tagToString =
  case _ of
    H1 -> "h1"
    H2 -> "h2"
    Div -> "div"
    Input -> "input"
    Span -> "span"
    Ul -> "ul"
    Li -> "li"
    Button -> "button"

applyAttributes :: forall msg . Store msg -> SimpleAttributes -> Array (EventHandler msg) -> DOM -> Effect Unit
applyAttributes equeue attrs eventHandlers dom = do
    -- TODO
      -- DOM.setAttribute dom "style" cssStr
      -- attachEventHandler dom equeue eventHandler
      pure unit

attachEventHandler :: forall msg . DOM -> Store msg -> EventHandler msg -> Effect Unit
attachEventHandler dom equeue =
  case _ of
    OnInputChange k ->
      DOM.attachChangeEvent dom (\newInputStr ->
        equeue # Store.send (k newInputStr)
      )
    OnClick msg ->
      DOM.attachClickEvent dom (equeue # Store.send msg)


fromVDOM :: forall msg . Store msg -> Html msg -> Effect DOM
fromVDOM equeue =
  case _ of
    Text str ->
      DOM.text str
    El tag attrs eventHandlers children -> do
      divDOM <- DOM.make (tagToString tag)
      -- apply the attributes
      _ <- divDOM # applyAttributes equeue attrs eventHandlers
      -- add the children
      _ <- children # traverse (\childElement -> do
        childDOM <- fromVDOM equeue childElement
        DOM.appendChild divDOM childDOM
      )
      pure divDOM



fromComponent :: forall model msg . (msg -> model -> Effect model) -> Effect model -> (model -> Html msg) -> DOM -> Effect Unit
fromComponent update initEff view root = do
  initModel <- initEff
  equeue <- Store.make (\_ -> update) initModel
  let initvdom = view initModel
  initdom <- fromVDOM equeue initvdom
  DOM.appendChild root initdom
  Timer.intervalLoop
    (Duration.fromMilliseconds 200)
    { dom: initdom, vdom: initvdom }
    (\{ dom, vdom } -> do
      model <- Store.getModel equeue
      let vdom' = view model
      -- TODO: Remove
      -- dom' <- fromVDOM equeue vdom'
      let patch = diff vdom (Just vdom')
      dom' <- dom # applyPatch patch
      pure { dom: dom', vdom: vdom' }
    )
  
-- ===Diffing===

data Patch msg =
    ReplaceWith (Html msg)
  | Remove
  | Update (Array AttributePatch) (Array (Patch msg))

data AttributePatch =
    RemoveAttribute AttributeName
  | ReplaceAttributeWith AttributeName AttributeValue


diff :: forall msg . Html msg -> Maybe (Html msg) -> Patch msg
diff vdom maybeVdom' =
  case maybeVdom' of
    Just vdom' ->
      case vdom, vdom' of
        Text _, _  ->
         ReplaceWith vdom'

        El _ _ _ _, Text _  ->
         ReplaceWith vdom'

        El tag attrs eventHandlers children, El tag' attrs' eventHandlers' children' ->
         if tag /= tag' then
           ReplaceWith vdom'
         else
           Update (diffAttr attrs attrs') (diffChildren children children')

    Nothing ->
      Remove

arrayFromMap :: forall k v . Map k v -> Array (Tuple k v)
arrayFromMap dict = Map.toUnfoldable dict


diffAttr :: forall msg . SimpleAttributes -> SimpleAttributes -> Array AttributePatch
diffAttr attrs attrs' =
  let attributeMapDiff = diffMap attrs attrs'
   in
   Array.concat
    [ attributeMapDiff.diffLeft
        # arrayFromMap
        # map (RemoveAttribute <<< Tuple.fst)
    , attributeMapDiff.intersection
        # arrayFromMap
        # map (\(Tuple.Tuple k { right }) -> ReplaceAttributeWith k right)
    , attributeMapDiff.diffRight
        # arrayFromMap
        # map (\(Tuple.Tuple k v) -> ReplaceAttributeWith k v)
    ]

diffMap :: forall k a . Ord k => Map k a -> Map k a -> { diffLeft :: Map k a, intersection :: Map k { left :: a, right :: a} , diffRight :: Map k a }
diffMap map map' =
  { diffLeft: Map.difference map map'
  , intersection: Map.intersectionWith (\a a' -> { left: a, right: a' }) map map'
  , diffRight: Map.difference map' map
  }

diffChildren :: forall msg . Array (Html msg) -> Array (Html msg) -> Array (Patch msg)
diffChildren children children' =
  let n  = Array.length children
      n' = Array.length children'
      commonPatches = Array.zipWith (\vdom vdom' -> Remove) children children'
   in
   if n <= n' then
     commonPatches <> ((Array.splitAt n children').after # map ReplaceWith)
   else
     -- n > n'
     commonPatches <> Array.replicate (n - n') Remove


applyPatch :: forall msg . Patch msg -> DOM -> Effect DOM
applyPatch patch dom =
  pure dom
