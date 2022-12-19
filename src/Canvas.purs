module Canvas
  ( CanvasRef
  , Path2D
  , CanvasConfig
  , makePath2D
  , stroke
  , fill
  , setFillStyle
  , clear
  , push
  , save
  , restore
  , setGlobalAlpha

  , rect
  , arc
  , lineTo
  , moveTo
  ) where

import Prelude
import Effect (Effect)

data CanvasRef

data Path2D

type CanvasConfig = { width :: Number, height :: Number  }


-- ===Canvas operations===
foreign import makePath2D :: Effect Path2D

foreign import stroke :: Path2D -> CanvasRef -> Effect Unit
foreign import fill :: Path2D -> CanvasRef -> Effect Unit
foreign import setFillStyle :: String -> CanvasRef -> Effect Unit
foreign import foreignClear :: Number -> Number -> CanvasRef -> Effect Unit

clear :: CanvasConfig -> CanvasRef -> Effect Unit
clear canvas = foreignClear canvas.width canvas.height

foreign import save :: CanvasRef -> Effect Unit
foreign import restore :: CanvasRef -> Effect Unit

push :: Effect Unit-> CanvasRef -> Effect Unit
push cmd canvasRef = do
  canvasRef # save
  cmd
  canvasRef # restore

foreign import setGlobalAlpha :: Number -> CanvasRef -> Effect Unit

-- ===Path2D operations===

-- x => y => width => height => path2d
foreign import rect :: Number -> Number -> Number -> Number -> Path2D -> Effect Unit
-- x => y => radius => angle0 => angle1 => path2d
foreign import arc :: Number -> Number -> Number -> Number -> Number -> Path2D -> Effect Unit

foreign import lineTo :: Number -> Number -> Path2D -> Effect Unit
foreign import moveTo :: Number -> Number -> Path2D -> Effect Unit
