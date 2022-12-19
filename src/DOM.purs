module DOM
  ( DOM
  , make
  , text
  , appendChild
  , root
  , getFirst
  , setAttribute
  , setTextContent
  , attachClickEvent
  , attachChangeEvent
  , replaceWith
  , makeCanvas
  ) where

import Prelude
import Effect (Effect)
import Data.Maybe (Maybe(..))
import Canvas ( CanvasRef, CanvasConfig )

data DOM

type QuerySelector = String

foreign import make :: String -> Effect DOM

foreign import text :: String -> Effect DOM

foreign import appendChild :: DOM -> DOM -> Effect Unit

foreign import root :: Effect DOM

foreign import getFirstExplicit :: (DOM -> Maybe DOM) -> Maybe DOM -> QuerySelector -> Effect (Maybe DOM)

getFirst :: QuerySelector -> Effect (Maybe DOM)
getFirst = getFirstExplicit Just Nothing

foreign import setAttribute :: DOM -> String -> String -> Effect Unit

foreign import setTextContent :: DOM -> String -> Effect Unit

foreign import attachClickEvent :: DOM -> Effect Unit -> Effect Unit

foreign import attachChangeEvent :: DOM -> (String -> Effect Unit) -> Effect Unit

foreign import replaceWith :: DOM -> DOM -> Effect Unit

foreign import getCanvasContext :: DOM -> Effect CanvasRef

makeCanvas :: CanvasConfig -> Effect { canvasRef :: CanvasRef, canvasDOM :: DOM }
makeCanvas { width, height } = do
  canvasDOM <- make "canvas"
  setAttribute canvasDOM "width" (show width)
  setAttribute canvasDOM "height" (show height)
  canvasRef <- getCanvasContext canvasDOM
  pure { canvasRef: canvasRef, canvasDOM: canvasDOM }
